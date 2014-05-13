#!/usr/bin/env lua

--[[
    
    wwd.lua

    Copyright (C) 2014 Bart Van Der Meerssche <bart@flukso.net>

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

]]--

local dbg = require "dbg"
local nixio = require "nixio"
local luci = require "luci"
luci.json = require "luci.json"
local uci = require "luci.model.uci".cursor()
local uloop = require "uloop"
uloop.init()
local ubus = require "ubus"
local ub = assert(ubus.connect(), "unable to connect to ubus")
local zlib = require "zlib"
local vstruct = require "vstruct"
vstruct.cache = true
local rfsm = require "rfsm"
rfsm.pp = require "rfsm.pp"
rfsm.timeevent = require "rfsm.timeevent"
local mosq = require "mosquitto"

local state, trans, conn, yield = rfsm.state, rfsm.trans, rfsm.conn, rfsm.yield

local DEBUG = {
	decode = false,
	encode = false
}

local DEVICE = uci:get_first("system", "system", "device")
local ULOOP_TIMEOUT_MS = 1e3
local O_RDWR_NONBLOCK = nixio.open_flags("rdwr", "nonblock")
local TIMESTAMP_MIN = 1234567890

-- mosquitto client params
local MOSQ_ID = DAEMON
local MOSQ_CLN_SESSION = true
local MOSQ_HOST = "localhost"
local MOSQ_PORT = 1883
local MOSQ_KEEPALIVE = 300
local MOSQ_TIMEOUT = 0 -- return instantly from select call
local MOSQ_MAX_PKTS = 10 -- packets
local MOSQ_QOS0 = 0
local MOSQ_QOS1 = 1
local MOSQ_RETAIN = true
local MOSQ_TOPIC_SENSOR_CONFIG = string.format("/device/%s/config/sensor", DEVICE)
local MOSQ_TOPIC_SENSOR = "/sensor/%s/%s"

-- connect to the MQTT broker
mosq.init()
local mqtt = mosq.new(MOSQ_ID, MOSQ_CLN_SESSION)
mqtt:connect(MOSQ_HOST, MOSQ_PORT, MOSQ_KEEPALIVE)

local UART_DEV = "/dev/ttyS0"
local UART_BUFFER_SIZE = 4096
local UART_SYNC_PATTERN = "\170\170\170\170" -- 0xaaaaaaaa
local UART_FMT_SEQ_LENGTH = "> @%d seq:u2 length:u2" -- big endian!
local UART_FMT_ADLER32 = "> @%d adler32:u4"
local UART_FMT_TLV = "> @%d typ:u2 length:u2"
local UART_FMT_PACKET = "> sync:s seq:u2 length:u2 t:u2 l:u2 v:s"

local UART_RX_ELEMENT = {
	[0] = { event = "e_rx_ping", fmt = "" },
	[1] = { event = "e_rx_basic_usage_data",
	        fmt = [[> time_index:u2 generated_power:u2 rpm:u2 energy_charged_to_battery:u2
	              battery_charge_status:u1 selected_profile:u1]] },
	[2] = { event = "e_rx_technical_usage_data",
	        fmt = [[> battery_voltage:u2 generator_voltage:u2 generator_current:u2
	              charge_current:u2 x_load_current:u2 mainboard_temperature:i2
	              x_load_temperature:i2]] },
	[3] = { event = "e_rx_power_consumption_data",
	        fmt = [[> highpower_output_voltage:u2 highpower_output_current:u2
	              QI_output_voltage:u2 QI_output_current:u2 USB_output_voltage:u2
	              USB_output_current:u2]] },
	[9] = "e_rx_error_warning_messages",
--	[10] = "e_rx_subscription_request",
--	[11] = "e_rx_version_info_request",
	[12] = { event = "e_rx_version_info",
	         fmt = [[> mainboard_embedded_sw_version:{ major:u1, minor:u1 }
	               ui_element_version:{ major: u1, minor: u1 }
	               mainboard_serial_number:u4]],
	         topic = "/device/%s/ww/version" },
--	[13] = "e_rx_statistics_info_request",
	[14] = { event = "e_rx_statistics_info", fmt = "" },
--	[15] = "e_rx_config_info_request",
	[16] = { event = "e_rx_battery type data", fmt = "" },
	[17] = { event = "e_rx_generator_type_data", fmt = "" },
	[18] = "e_rx_embedded_system_status_update",
--	[19] = "e_rx_upgrade_request",
	[20] = "e_rx_upgrade_confirmation",
	[21] = "e_rx_upgrade_reject",
	[22] = "e_rx_shutdown_request",
--	[23] = "e_rx_shutdown_confirmation",
--	[24] = "e_rx_shutdown_reject",
--	[251] = "e_rx_open_debug_interface",
--	[252] = "e_rx_close_debug_interface",
--	[253] = "e_rx_debug_interface_data_in",
	[254] = "e_rx_debug_interface_data_out",
	[255] = "e_rx_pong"
}

local UART_TX_ELEMENT = {
	ping = { typ = 0, fmt = "" },
	subscription_request = { typ = 10, fmt = [=[[1| x5 power_consumption_data:b1
	    technical_usage_data:b1 basic_usage_data:b1]]=] },
	version_info_request = { typ = 11, fmt = "" },
	statistics_info_request = { typ = 13, fmt = "" },
	config_info_request = { typ = 15, fmt = "" },
	pong = { typ = 255, fmt = "" },
}

local uart = {
	fd = nixio.open(UART_DEV, O_RDWR_NONBLOCK),
	buffer = nil,
	hex = nil,
	head = nil,
	tail = nil,
	seq = nil,
	length = nil,
	adler32 = nil,
	payload = nil,
	tlv_head = nil,
	seq_out = 0,

	debug = function(self, ...)
		if DEBUG.decode and self.buffer then
			self.hex = nixio.bin.hexlify(self.buffer)
			dbg.vardump(self)
		end
		return ...
	end,

	fileno = function(self)
		return self.fd:fileno()
	end,

	flush = function(self)
		while self.fd:read(UART_BUFFER_SIZE) do end
	end,

	read = function(self)
		if self.buffer then
			self.buffer = self.buffer .. self.fd:read(UART_BUFFER_SIZE)
		else
			self.buffer = self.fd:read(UART_BUFFER_SIZE)
		end
	end,

	packet = function(self)
		if not self.buffer or #self.buffer < 16 then return self:debug(false) end
		self.head = self.buffer:find(UART_SYNC_PATTERN)
		if not self.head then
			self.buffer = nil
			return self:debug(false)
		end

		local fmt = UART_FMT_SEQ_LENGTH:format(self.head - 1 + 4)
		vstruct.unpack(fmt, self.buffer, self)
		-- partial packet?
		if not (self.length and #self.buffer >= (self.head - 1) + 12 + self.length) then
			return self:debug(false)
		end

		self.tail = self.head + 11 + self.length
		fmt = UART_FMT_ADLER32:format(self.head - 1 + 8 + self.length)
		vstruct.unpack(fmt, self.buffer, self)
		if self.adler32 ~= zlib.adler32()(self.buffer:sub(self.head + 4, self.tail - 4)) then
			self.buffer = self.buffer:sub(self.tail + 1, -1)
			return self:debug(false)
		end

		self.tlv_head = self.head + 8
		return self:debug(true)
	end,

	tlv = function(self)
		return function() -- iterator
			if self.tlv_head > self.tail - 4 then
				-- pop packet
				self.buffer = self.buffer:sub(self.tail + 1, -1)
				return nil
			end

			local tlv = {}
			local fmt = UART_FMT_TLV:format(self.tlv_head - 1)
			vstruct.unpack(fmt, self.buffer, tlv)
			if not tlv.typ then return nil end --TODO raise error
			tlv.tail = self.tlv_head - 1 + 4 + tlv.length
			tlv.value = self.buffer:sub(self.tlv_head + 4, tlv.tail)
			self.tlv_head = tlv.tail + 1

			if DEBUG.decode	then
				tlv.hex = nixio.bin.hexlify(tlv.value)
				dbg.vardump(tlv)
			end
			return { t = tlv.typ, l = tlv.length, v = tlv.value }
		end
	end,

	write = function(self, elmt, data)
		local fields = {
			sync = UART_SYNC_PATTERN,
			seq = self.seq_out,
			t = UART_TX_ELEMENT[elmt].typ,
			v = vstruct.pack(UART_TX_ELEMENT[elmt].fmt, data or { })
		}
		fields.l = #fields.v
		fields.length = fields.l + 4
		local packet = vstruct.pack(UART_FMT_PACKET, fields)
		local adler32 = zlib.adler32()(packet:sub(5, -1))
		local adler32_pack = vstruct.pack(UART_FMT_ADLER32:format(0), { adler32 = adler32 })
		local packet = packet .. adler32_pack
		self.fd:write(packet)
		self.seq_out = (self.seq_out + 1) % 65536

		if DEBUG.encode then print("[tx] = " .. nixio.bin.hexlify(packet)) end
	end
}

local SENSOR = {
	generated_power = {
		typ = "electricity",
		unit = "W",
		data_type = "gauge"
	},
	rpm = {
		typ = "velocity",
		unit = "Hz",
		data_type = "gauge"
	},
	energy_charged_to_battery = {
		typ = "electricity",
		unit = "W",
		data_type = "gauge"
	},
	battery_charge_status = {
		typ = "fraction",
		unit = "%",
		data_type = "gauge"
	},
	selected_profile = {
		typ = "profile",
		unit = "",
		data_type = "gauge"
	},
	battery_voltage = {
		typ = "voltage",
		unit = "mV",
		data_type = "gauge"
	},
	generator_voltage = {
		typ = "voltage",
		unit = "mV",
		data_type = "gauge"
	},
	generator_current = {
		typ = "current",
		unit = "mA",
		data_type = "gauge"
	},
	charge_current = {
		typ = "current",
		unit = "mA",
		data_type = "gauge"
	},
	x_load_current = {
		typ = "current",
		unit = "mA",
		data_type = "gauge"
	},
	mainboard_temperature = {
		typ = "temperature",
		unit = "0.1°C", --TODO implement scaling
		data_type = "gauge"
	},
	x_load_temperature = {
		typ = "temperature",
		unit = "0.1°C", --TODO implement scaling
		data_type = "gauge"
	},
	highpower_output_voltage = {
		typ = "voltage",
		unit = "mV",
		data_type = "gauge"
	},
	highpower_output_current = {
		typ = "current",
		unit = "mA",
		data_type = "gauge"
	},
	QI_output_voltage = {
		typ = "voltage",
		unit = "mV",
		data_type = "gauge"
	},
	QI_output_current = {
		typ = "current",
		unit = "mA",
		data_type = "gauge"
	},
	USB_output_voltage = {
		typ = "voltage",
		unit = "mV",
		data_type = "gauge"
	},
	USB_output_current = {
		typ = "current",
		unit = "mA",
		data_type = "gauge"
	},
	total_time_used = {
		typ = "time",
		unit = "s",
		data_type = "counter"
	},
	total_energy_generated = {
		typ = "electricity",
		unit = "J",
		data_type = "counter"
	},
}


local sensor = {
	config = SENSOR,

	provision = function(self)
		local function num_entries(T)
			local i = 0
			for _ in pairs(T) do i = i + 1 end
			return i 
		end

		local function get_free_sensors(list)
			local range = list.main and list.main.max_provisioned_sensors
			if not range then return nil end

			local first_free, total_free = nil, range

			for i = 1, range do
				if list[tostring(i)] and list[tostring(i)].enable then
					total_free = total_free - 1
				elseif not first_free then
					first_free = i
				end
			end

			return first_free and tostring(first_free), total_free
		end

		local flukso = uci:get_all("flukso")
		local first_free, total_free = get_free_sensors(flukso)
		if num_entries(self.config) > total_free then return false end

		for sname, sprop in pairs(self.config) do
			local sidx = get_free_sensors(flukso)
			local values = {
				["class"] = "ww",
				["type"] = sprop.typ,
				["function"] = sname,
				["enable"] = 1
			}

			uci:tset("flukso", sidx, values)
			uci:save("flukso")
			flukso = uci:get_all("flukso")
		end
		uci:commit("flukso")
		return true
	end,

	load_cfg = function(self)
		local flukso = uci:get_all("flukso")
		for sidx, sprop in pairs(flukso) do
			if sprop["function"]  and self.config[sprop["function"]] then
				self.config[sprop["function"]].id = sprop.id
			end
		end
	end,

	publish = function(self, elmnt)
		local timestamp = os.time()
		if timestamp < TIMESTAMP_MIN then return end --TODO raise error

		local data = { }
		vstruct.unpack(UART_RX_ELEMENT[elmnt.t].fmt, elmnt.v, data) 
		for sname, svalue in pairs(data) do
			local cfg = self.config[sname]
			if cfg and cfg.id then
				local topic = string.format(MOSQ_TOPIC_SENSOR, cfg.id, cfg.data_type)
				local payload = luci.json.encode({ timestamp, svalue, cfg.unit })
				mqtt:publish(topic, payload, MOSQ_QOS0, MOSQ_RETAIN)
			end
		end
	end,

	publish_cfg = function(self)
		local function config_clean(itbl)
			local otbl = luci.util.clone(itbl, true)
			for section, section_tbl in pairs(otbl) do
				section_tbl[".index"] = nil
				section_tbl[".name"] = nil
				section_tbl[".type"] = nil
				section_tbl[".anonymous"] = nil

				for option, value in pairs(section_tbl) do
					section_tbl[option] = tonumber(value) or value
					if type(value) == "table" then -- we're dealing with a list
						for i in pairs(value) do
							value[i] = tonumber(value[i]) or value[i]
						end 
					end
				end
			end

			return otbl
		end

		local flukso = luci.json.encode(config_clean(uci:get_all("flukso")))
		mqtt:publish(MOSQ_TOPIC_SENSOR_CONFIG, flukso, MOSQ_QOS0, MOSQ_RETAIN)
	end
}

local ww = {
	publish = function(self, elmnt)
		local data = { }
		vstruct.unpack(UART_RX_ELEMENT[elmnt.t].fmt, elmnt.v, data)
		local topic = string.format(UART_RX_ELEMENT[elmnt.t].topic, DEVICE)
		mqtt:publish(topic, luci.json.encode(data), MOSQ_QOS0, MOSQ_RETAIN)
	end,
}

-- define gettime function for rFSM
local function rfsm_gettime()
	local secs, usecs = nixio.gettimeofday()
	return secs, usecs * 1e3
end

rfsm.timeevent.set_gettime_hook(rfsm_gettime)

-- rFSM event arguments and state context
local e_arg, s_ctx

local root = state {
	dbg = false,
	warn = function(...)
		local msg = table.concat({...}, " ")
		error(msg, 0)
	end,
	err = function(...)
		local msg = table.concat({...}, " ")
		error(msg, 0)
	end,

	load_config = state {
		entry = function()
			sensor:load_cfg()
			sensor:publish_cfg()
		end
	},

	receiving = state {
		entry = function()
		end
	},

	provision = state {
		entry = function()
			e_arg(sensor:provision())
		end
	},

	ping = state { -- outgoing ping
		entry = function()
			s_ctx = e_arg -- store cb function in ctx
			uart:write("ping")
		end
	},

	pong = state { -- incoming ping
		entry = function()
			uart:write("pong")
		end
	},

	data = state {
		entry = function()
			sensor:publish(e_arg)
		end
	},

	subscription_request = state {
		entry = function()
			uart:write("subscription_request", e_arg)
		end
	},

	version_info_request = state {
		entry = function()
			uart:write("version_info_request")
		end
	},

	statistics_info_request = state {
		entry = function()
			uart:write("statistics_info_request")
		end
	},

	config_info_request = state {
		entry = function()
			uart:write("config_info_request")
		end
	},

	response = state {
		entry = function()
			ww:publish(e_arg)
		end
	},

	trans { src = "initial", tgt = "load_config" },
	trans { src = "load_config", tgt = "receiving", events = { "e_done" } },
	trans { src = "receiving", tgt = "provision", events = { "e_provision" } },
	trans { src = "provision", tgt = "load_config", events = { "e_done" } },
	trans { src = "receiving", tgt = "ping", events = { "e_tx_ping" } },
	trans { src = "ping", tgt = "receiving", events = { "e_rx_pong" },
		effect = function() s_ctx(true) end
	},
	trans { src = "ping", tgt = "receiving", events = { "e_after(1)" },
		effect = function() s_ctx(false) end
	},
	trans { src = "receiving", tgt = "pong", events = { "e_rx_ping" } },
	trans { src = "pong", tgt = "receiving", events = { "e_done" } },
	trans { src = "receiving", tgt = "data", events = {
		"e_rx_basic_usage_data" ,
		"e_rx_technical_usage_data",
		"e_rx_power_consumption_data",
		"e_rx_statistics_info" }
	},
	trans { src = "data", tgt = "receiving", events = { "e_done" } },
	trans { src = "receiving", tgt = "subscription_request", events = { "e_subscription_request" } },
	trans { src = "subscription_request", tgt = "receiving", events = { "e_done" } },
	trans { src = "receiving", tgt = "version_info_request", events = { "e_version_info_request" } },
	trans { src = "version_info_request", tgt = "receiving", events = { "e_done" } },
	trans { src = "receiving", tgt = "statistics_info_request", events = { "e_statistics_info_request" } },
	trans { src = "statistics_info_request", tgt = "receiving", events = { "e_done" } },
	trans { src = "receiving", tgt = "config_info_request", events = { "e_config_info_request" } },
	trans { src = "config_info_request", tgt = "receiving", events = { "e_done" } },
	trans { src = "receiving", tgt = "response", events = {
		"e_rx_version_info" }
	},
	trans { src = "response", tgt = "receiving", events = { "e_done" } },
}


local fsm = rfsm.init(root)
rfsm.run(fsm)

local event = {
	lock = false,
	head = 1,
	tail = 0,
	queue = { },

	is_queue_empty = function(self)
		return self.tail < self.head
	end,

	push = function(self, e, arg, cb)
		if e then
			self.tail = self.tail + 1
			self.queue[self.tail] = { e, arg, cb }
		end
	end,

	pop = function(self)
		if self:is_queue_empty() then return nil end

		local t = self.queue[self.head]
		self.queue[self.head] = nil
		self.head = self.head + 1
		return t[1], t[2], t[3]
	end,

	run = function(self)
		if not self.lock then
			self.lock = true
			while not self:is_queue_empty() do
				local e, arg, cb = self:pop()
				e_arg = arg -- set the event arg for the fsm
				rfsm.send_events(fsm, e)
				rfsm.run(fsm)
				if type(cb) == "function" then cb() end -- run the completion callback
			end
			rfsm.run(fsm) -- service the fsm timers even if no events are queued
			self.lock = false
		end
	end,

	process = function(self, e, arg, cb) -- external method
		self:push(e, arg, cb)
		self:run()
	end
}

local ub_methods = {
	["flukso.ww"] = {
		debug = {
			function(req, msg)
				if type(msg.fsm) == "boolean" then
					if msg.fsm then
						fsm.dbg = rfsm.pp.gen_dbgcolor("ww", {
							STATE_ENTER = true,
							STATE_EXIT = true,
							EFFECT = false,
							DOO = false,
							EXEC_PATH = false,
							ERROR = true,
							HIBERNATING = false,
							RAISED = true,
							TIMEEVENT = false
						}, false)
					else
						fsm.dbg = function() return end
					end
				end

				if type(msg.decode) == "boolean" then
					DEBUG.decode = msg.decode
				end

				if type(msg.encode) == "boolean" then
					DEBUG.encode = msg.encode
				end

				ub:reply(req, { success = true, msg = "wwd debugging flags updated" })
			end, { fsm = ubus.BOOLEAN, decode = ubus.BOOLEAN, encode = ubus.BOOLEAN }
		},

		provision = {
			function(req, msg)
				event:process("e_provision", function(success)
					local reply = success and "sensors povisioned" or "not enough free sensors"
					ub:reply(req, { success = success, msg = reply })
				end)
			end, { }
		},

		subscription_request = {
			function(req, msg)
				subs = { -- defaults
					basic_usage_data = true,
					technical_usage_data = false,
					power_consumption_data = false
				}

				if type(msg.basic) == "boolean" then
					subs.basic_usage_data = msg.basic
				end

				if type(msg.tech) == "boolean" then
					subs.technical_usage_data = msg.tech
				end

				if type(msg.power) == "boolean" then
					subs.power_consumption_data = msg.power
				end

				event:process("e_subscription_request", subs, function()
					local reply = "subscription requests updated"
					ub:reply(req, { success = true, msg = reply })
				end)
			end, { basic = ubus.BOOLEAN, tech = ubus.BOOLEAN, power = ubus.BOOLEAN }
		}
	}
}

ub:add(ub_methods)

local ub_events = {
	["flukso.ww.event"] = function(msg)
		if type(msg.event) == "string" then
			event:process(msg.event, msg.arg)
		end
	end,

	["flukso.ww.ping"] = function(msg)
		event:process("e_tx_ping", function(success)
			local reply = success and "pong!" or "ping failed"
			ub:send("flukso.ww.pong", { success = success, msg = reply })
		end)
	end
}

ub:listen(ub_events)

local ut
ut = uloop.timer(function()
		ut:set(ULOOP_TIMEOUT_MS)
		-- service the rFSM timers
		event:process()
		-- service the mosquitto loop
		if not mqtt:loop(MOSQ_TIMEOUT, MOSQ_MAX_PKTS) then
			mqtt:reconnect()
		end
	end, ULOOP_TIMEOUT_MS)

uart:flush()
local ufd = uloop.fd(uart:fileno(), uloop.READ, function(events)
		uart:read()
		while uart:packet() do
			for elmt in uart:tlv() do
				event:process(UART_RX_ELEMENT[elmt.t].event, elmt)
			end
		end
	end)

uloop:run()
