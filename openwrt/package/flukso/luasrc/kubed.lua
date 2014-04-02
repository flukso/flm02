#!/usr/bin/env lua

--[[
    
    kubed.lua - The FluksoKube daemon

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
nixio.util = require "nixio.util"
local luci = require "luci"
luci.json = require "luci.json"
luci.util = require "luci.util"
local uci = require "luci.model.uci".cursor()
local uloop = require "uloop"
uloop.init()
local ubus = require "ubus"
local ub = assert(ubus.connect(), "unable to connect to ubus")
local vstruct = require "vstruct"
vstruct.cache = true
local rfsm = require "rfsm"
rfsm.pp = require "rfsm.pp"
rfsm.timeevent = require "rfsm.timeevent"
local mosq = require "mosquitto"

local function rfsm_gettime()
	local secs, usecs = nixio.gettimeofday()
	return secs, usecs * 1e3
end

rfsm.timeevent.set_gettime_hook(rfsm_gettime)

local hex, unhex = nixio.bin.hexlify, nixio.bin.unhexlify
local state, trans, conn = rfsm.state, rfsm.trans, rfsm.conn

local DAEMON = os.getenv("DAEMON") or "kubed"
local DEVICE = uci:get_first("system", "system", "device")
local KUBE, SENSOR, REGISTRY, FIRMWARE, DECODE
local FIRMWARE_BLOCK_SIZE = 64
local FF = string.char(255)
local O_RDONLY = nixio.open_flags("rdonly")
local ULOOP_TIMEOUT_MS = 1000
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

local MOSQ_TOPIC_KUBE_CONFIG = string.format("/device/%s/config/kube", DEVICE)
local MOSQ_TOPIC_SENSOR_CONFIG = string.format("/device/%s/config/sensor", DEVICE)
local MOSQ_TOPIC_SENSOR = "/sensor/%s/%s"

local FMT_CMD_SET_GROUP = "sg %s"

-- bootloader packet formats
local FMT_HEADER = "< grp:u1 [1| ctl:b1 dst:b1 ack:b1 nid:u5] len:u1"
local FMT_PREFIX = "@3 "
local FMT_PAIR_REQUEST = "< hw_type:u2 grp:u1 nid:u1 check:u2 hw_id:s16"
local FMT_PAIR_REPLY = "< hw_type:u2 grp:u1 nid:u1 key:s16"
local FMT_UPGRADE_REQUEST = "< hw_type:u2 sw_version:u2 sw_size:u2 sw_crc:u2"
local FMT_UPGRADE_REPLY = FMT_UPGRADE_REQUEST
local FMT_DOWNLOAD_REQUEST = "< sw_version:u2 sw_index:u2"
local FMT_DOWNLOAD_REPLY = "< sw_xor:u2 sw_block:s"

local DEBUG = {
	config = false,
	decode = false,
	encode = false
}

-- connect to the MQTT broker
mosq.init()
local mqtt = mosq.new(MOSQ_ID, MOSQ_CLN_SESSION)
mqtt:connect(MOSQ_HOST, MOSQ_PORT, MOSQ_KEEPALIVE)

local function load_config()
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

	local function load_scaling_functions(hw_entry)
		-- define a read-only proxy table, see PiL p.127
		local function set_readonly(t)
			local proxy = { }
			local mt = {
				__index = t,
				__newindex = function(t, k, v)
					error("Attempt to update a read-only table", 2)
				end,
				__metatable = false
			}
			setmetatable(proxy, mt)
			return proxy
		end

		for k, v in pairs(hw_entry) do
			if tonumber(k) then -- we're dealing with a software entry
				for sensor, params in pairs(v.decode.sensors) do
					if params.scale then
						local chunk = "local x = ...; return " .. params.scale
						params.scale = assert(loadstring(chunk))
						local env = set_readonly({ math = math })
						setfenv(params.scale, env)
					end
				end
			end
		end
	end

	KUBE = uci:get_all("kube")
	SENSOR = uci:get_all("flukso")
	REGISTRY, FIRMWARE, DECODE = { }, { }, { }

	for file in nixio.fs.dir(KUBE.main.cache .. "/registry") do
		local path = string.format("%s/registry/%s", KUBE.main.cache, file)
		local fd = nixio.open(path, O_RDONLY)
		local registry = luci.json.decode(fd:readall())
		fd:close()

		if registry then
			for k, v in pairs(registry) do
				REGISTRY[k] = v
				load_scaling_functions(v)
			end
		else
			--TODO raise error when file is not json decodable
		end
	end

	--TODO raise error when KUBE, SENSOR or REGISTRY are nil

	local kube = luci.json.encode(config_clean(KUBE))
	mqtt:publish(MOSQ_TOPIC_KUBE_CONFIG, kube, MOSQ_QOS0, MOSQ_RETAIN)
	local sensor = luci.json.encode(config_clean(SENSOR))
	mqtt:publish(MOSQ_TOPIC_SENSOR_CONFIG, sensor, MOSQ_QOS0, MOSQ_RETAIN)

	if DEBUG.config then
		dbg.vardump(KUBE)
		dbg.vardump(SENSOR)
		dbg.vardump(REGISTRY)
	end
end

local function add_kube_to_decode_cache(kid, hw_type, sw_version)
	if not DECODE[kid] then DECODE[kid] = { }  end
	DECODE[kid][sw_version] = luci.util.clone(
		REGISTRY[tostring(hw_type)][tostring(sw_version)].decode, true)

	for sidx, params in pairs(SENSOR) do
		if tonumber(sidx) and tonumber(params.kid) == kid then
			DECODE[kid][sw_version]["sensors"][params["type"]].sid = params.id
		end
	end

	if DEBUG.decode then dbg.vardump(DECODE) end
    return true
end

local function set_group(grp)
	local cmd = string.format(FMT_CMD_SET_GROUP, grp)
	ub:call("flukso.flx", "ctrl", { cmd = cmd })
end

local function decode(script, pkt)
	if type(script) == "string" then
		vstruct.unpack(FMT_PREFIX .. script, pkt.bin, pkt.pld)
	elseif type(script) == "table" then
		if script.type_bits == 0 then
			local len_s = tostring(pkt.hdr.len)
			if not script[len_s] then return end --TODO raise error
			vstruct.unpack(FMT_PREFIX .. script[len_s], pkt.bin, pkt.pld)
		elseif script.type_bits % 8 ~= 0 then return --TODO raise error
		else
            local type_bytes = script.type_bits / 8
			local fmt_type = string.format("< u%s", type_bytes)
			local pkt_type_s = table.concat(vstruct.unpack(FMT_PREFIX .. fmt_type, pkt.bin))
			if not script[pkt_type_s] then return end --TODO raise error
			local fmt_prefix = string.format("@%s ", 3 + type_bytes)
			vstruct.unpack(fmt_prefix .. script[pkt_type_s], pkt.bin, pkt.pld)
		end
	end

	if DEBUG.decode then dbg.vardump(pkt.pld) end
end

local function encode(format, data)
	return vstruct.pack(format, data)
end

local function scale(script, pkt)
	for sensor_t, value in pairs(pkt.pld) do
		local scl = script.sensors[sensor_t].scale
		if type(scl) == "function" then value = scl(value) end
		pkt.pld[sensor_t] = value
	end

	if DEBUG.decode then dbg.vardump(pkt.pld) end
end

local function publish(script, pkt)
	local timestamp = os.time()
	if timestamp < TIMESTAMP_MIN then return end --TODO raise error
	local mosq_qos = (pkt.hdr.ack and MOSQ_QOS1) or MOSQ_QOS0

	for sensor_t, value in pairs(pkt.pld) do
		local sid = script.sensors[sensor_t].sid
		--data_type is gauge or counter
		local data_t = script.sensors[sensor_t].data_type
		local topic = string.format(MOSQ_TOPIC_SENSOR, sid, data_t)
		local unit = script.sensors[sensor_t].unit
		local payload = luci.json.encode({ timestamp, value, unit })
		mqtt:publish(topic, payload, mosq_qos, MOSQ_RETAIN)
	end
end

local function send(hdr, format, pld)
	local pld_enc = encode(format, pld)
	hdr.len = #pld_enc
	local hdr_enc = encode(FMT_HEADER, hdr)
	local pkt = hdr_enc .. pld_enc

	if DEBUG.encode then
		dbg.vardump(hdr)
		dbg.vardump(pld)
	end

	return ub:send("flukso.kube.packet.tx", { hex = hex(pkt) })
end

local function reply(hdr, format, pld)
	if hdr.ctl and hdr.ack then --OAM packet
		hdr.dst = not hdr.dst
	end

	return send(hdr, format, pld)
end

local function is_not_kube_provisioned(hw_id_hex)
	for kid, values in pairs(KUBE) do
		if values.hw_id and values.hw_id == hw_id_hex then
			return false
		end
	end

	return true
end

local function latest_kube_firmware(hw_type_s)
	local latest = 0

	for k, v in pairs(REGISTRY[hw_type_s]) do
		if tonumber(k) and tonumber(k) > latest then
			latest = tonumber(k)
		end
	end

	return tostring(latest)
end

local function provision_kube(hw_id_hex, hw_type_s)
	local function is_hw_type_registered(hw_type_s)
		return REGISTRY[hw_type_s]
	end

	local function sensor_count(hw_type_s)
		local total = 0
		local sw_version_s = latest_kube_firmware(hw_type_s)
		for k, v in pairs(REGISTRY[hw_type_s][sw_version_s].decode.sensors) do
			total = total + 1
		end

		return total
	end

	-- works for both KUBE and SENSOR data
	local function get_free(list)
		local range = list.main and
			(list.main.max_provisioned_sensors or list.main.max_nodes)
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

	local function update_recycle(sidx_s)
		local recycle = SENSOR[sidx_s].recycle
		return tostring((recycle and tonumber(recycle) + 1) or 0)
	end

	local kid_s = get_free(KUBE)
	local sidx_s, free_sensors = get_free(SENSOR)

	if not is_hw_type_registered(hw_type_s) then
		--TODO raise error
	elseif not kid_s then
		--TODO raise error
	elseif sensor_count(hw_type_s) > free_sensors then
		--TODO raise error
	else
		local values = {
			hw_id = hw_id_hex,
			hw_type = hw_type_s,
			sw_version = 0,
			enable = 1
		}

		uci:section("kube", "node", kid_s, values)
		uci:save("kube")
		uci:commit("kube")
		KUBE = uci:get_all("kube")

		for sensor_type_s in pairs(REGISTRY[hw_type_s]
			[latest_kube_firmware(hw_type_s)].decode.sensors) do
			sidx_s = get_free(SENSOR)

			local values = {
				class = "kube",
				["type"] = sensor_type_s,
				kid = kid_s,
				recycle = update_recycle(sidx_s),
				enable = 1
			}

			uci:tset("flukso", sidx_s, values)
			uci:save("flukso")
			SENSOR = uci:get_all("flukso")
		end
		uci:commit("flukso")

		return tonumber(kid_s)
	end

	return nil
end

local function deprovision_kube(kid_s)
	uci:foreach("flukso", "sensor", function(sensor)
		if sensor.kid == kid_s then
			local sidx = sensor[".name"]
			local entries = { "class", "type", "function", "kid", "enable" }
			for i, entry in ipairs(entries) do
				uci:delete("flukso", sidx, entry)
			end
		end
	end)
	uci:save("flukso")
	uci:commit("flukso")

	uci:delete("kube", kid_s)
	uci:save("kube")
	uci:commit("kube")
end

local function is_kid_allocated(kid_s, hw_type_s)
	if hw_type_s then
		return type(KUBE[kid_s]) == "table" and KUBE[kid_s].hw_type == hw_type_s
	else
		return type(KUBE[kid_s]) == "table"
	end
end

local function get_kid(hw_id_hex)
	for kid, values in pairs(KUBE) do
		if values.hw_id and values.hw_id == hw_id_hex then
			return tonumber(kid)
		end
	end

	return nil
end

local function update_kube_sw_version(kid_s, sw_version_s)
	if KUBE[kid_s].sw_version ~= sw_version_s then
		uci:set("kube", kid_s, "sw_version", sw_version_s)
		uci:save("kube")
		uci:commit("kube")
		KUBE = uci:get_all("kube")
	end
end

local function load_kube_sw(hw_type, sw_version)
	if not FIRMWARE[hw_type] then FIRMWARE[hw_type] = { } end
	if FIRMWARE[hw_type][sw_version] then return true end

	local path = string.format("%s/firmware/%d.%d", KUBE.main.cache, hw_type, sw_version)
	local fd = nixio.open(path, O_RDONLY)
	if not fd then return false end --TODO raise error
	local firmware = fd:readall()
	fd:close()

	local padding = FIRMWARE_BLOCK_SIZE - firmware:len() % FIRMWARE_BLOCK_SIZE
	if (padding < FIRMWARE_BLOCK_SIZE) then
		firmware = firmware .. string.rep(FF, padding)
	end

	FIRMWARE[hw_type][sw_version] = {
		size = firmware:len() / 16,
		crc16 = nixio.bin.crc16(firmware),
		bin = nixio.bin.white(firmware)
	}

	return true
end

local function kube_target_sw(hw_type)
	local sw_version = tonumber(latest_kube_firmware(tostring(hw_type)))
	if not load_kube_sw(hw_type, sw_version) then return nil end
	local sw_size = FIRMWARE[hw_type][sw_version].size
	local sw_crc = FIRMWARE[hw_type][sw_version].crc16
	return sw_version, sw_size, sw_crc
end

local function get_firmware_block(hw_type, sw_version, sw_index)
	if not load_kube_sw(hw_type, sw_version) then return nil end
	local len = FIRMWARE[hw_type][sw_version]["bin"]:len()
	local start = sw_index * FIRMWARE_BLOCK_SIZE + 1
	if start > len then return nil end
	local finish = (sw_index + 1) * FIRMWARE_BLOCK_SIZE
	if finish > len then finish = len end
	return FIRMWARE[hw_type][sw_version]["bin"]:sub(start, finish)
end

-- rFSM event parameters
local e_arg

local root = state {
	dbg = false,

	collecting = state {
		entry = function()
				load_config()
				set_group(KUBE.main.collect_group)
		end,

		-- rFSM does not support internal transitions
		-- resorting to a nested state without entry/exit actions as a workaround
		receiving = state { },
		decode = state {
			entry = function()
				local kid = e_arg.hdr.nid
				local kid_s = tostring(kid)
				if not KUBE[kid_s] then return end -- TODO raise error
				local hw_type = tonumber(KUBE[kid_s].hw_type)
				local sw_version = tonumber(KUBE[kid_s].sw_version)
				if not (DECODE[kid] and DECODE[kid][sw_version]) then
					if not add_kube_to_decode_cache(kid, hw_type, sw_version) then
						return --TODO raise error
					end
				end

				decode(DECODE[kid][sw_version], e_arg)
				scale(DECODE[kid][sw_version], e_arg)
				publish(DECODE[kid][sw_version], e_arg)
			end
		},
		upgrade = state {
			entry = function()
				decode(FMT_UPGRADE_REQUEST, e_arg)
				local kid_s = tostring(e_arg.hdr.nid)
				local hw_type_s = tostring(e_arg.pld.hw_type)
				if not is_kid_allocated(kid_s, hw_type_s) then return end --TODO raise error
				local sw_version_s = tostring(e_arg.pld.sw_version)
				update_kube_sw_version(kid_s, sw_version_s)

				local pld = { hw_type = e_arg.pld.hw_type }
				pld.sw_version, pld.sw_size, pld.sw_crc = kube_target_sw(e_arg.pld.hw_type)
				if pld.sw_version then reply(e_arg.hdr, FMT_UPGRADE_REPLY, pld) end
			end
		},
		download = state {
			entry = function()
				decode(FMT_DOWNLOAD_REQUEST, e_arg)
				local kid_s = tostring(e_arg.hdr.nid)
				if not is_kid_allocated(kid_s) then return end --TODO raise error
				local hw_type = tonumber(KUBE[kid_s].hw_type)
				local sw_version = e_arg.pld.sw_version
				local sw_index = e_arg.pld.sw_index

				local pld = {
					sw_xor = nixio.bit.bxor(sw_version, sw_index),
					sw_block = get_firmware_block(hw_type, sw_version, sw_index)
				}

				if pld.sw_block then
					reply(e_arg.hdr, FMT_DOWNLOAD_REPLY, pld)
				end
			end
		},
		trans { src = "initial", tgt = "receiving" },
		trans { src = "receiving", tgt = "decode", events = { "e_packet" } },
		trans { src = "decode", tgt = "receiving", events = { "e_done" } },
		trans { src = "receiving", tgt = "upgrade", events = { "e_upgrade_request" } },
		trans { src = "upgrade", tgt = "receiving", events = { "e_done" } },
		trans { src = "receiving", tgt = "download", events = { "e_download_request" } },
		trans { src = "download", tgt = "receiving", events = { "e_done" } }
	},

	pairing = state {
		entry = function()
				set_group(KUBE.main.pair_group)
			end,

		receiving = state { },
		pair_request = state {
			entry = function()
				decode(FMT_PAIR_REQUEST, e_arg)
			end
		},
		pair_reply = state {
			entry = function()
				local kid = get_kid(hex(e_arg.pld.hw_id))

				if kid then
					local pld = {
						hw_type = tonumber(KUBE[tostring(kid)].hw_type),
						grp = tonumber(KUBE.main.collect_group),
						nid = kid,
						key = KUBE.main.key
					}

					-- little hack to make sure the radio collects the upgrade request
					set_group(KUBE.main.collect_group)
					reply(e_arg.hdr, FMT_PAIR_REPLY, pld)
				end
			end
		},
		provision = state {
			entry = function()
				provision_kube(hex(e_arg.pld.hw_id), tostring(e_arg.pld.hw_type))
			end
		},

		trans { src = "initial", tgt = "receiving" },
		trans { src = "receiving", tgt = "pair_request", events = { "e_pair_request" } },
		trans { src = "pair_request", tgt = "provision", events = { "e_done" }, pn = 1,
			guard = function()
				return is_not_kube_provisioned(hex(e_arg.pld.hw_id))
			end
		},
		trans { src = "pair_request", tgt = "pair_reply", events = { "e_done" }, pn = 0 },
		trans { src = "provision", tgt = "pair_reply", events = { "e_done" } },
	},

	deprovision = state {
		entry = function()
			deprovision_kube(tostring(e_arg))
		end
	},

	trans { src = "initial", tgt = "collecting" },
	trans { src = "collecting", tgt = "pairing", events = { "e_pair" } },
	trans { src = "pairing", tgt = "collecting", events = { "e_after(30)" } },
	trans { src = ".pairing.pair_reply", tgt = "collecting", events = { "e_done" } },
	trans { src = "collecting", tgt = "deprovision", events = { "e_deprovision" },
		guard = function()
			return type(e_arg) == "number" and is_kid_allocated(tostring(e_arg))
		end
	},
	trans { src = "deprovision", tgt = "collecting", events = { "e_done" } }
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
	["flukso.kube"] = {
		debug = {
			function(req, msg)
				if type(msg.fsm) == "boolean" then
					if msg.fsm then
						fsm.dbg = rfsm.pp.gen_dbgcolor("kubed", {
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

				if type(msg.config) == "boolean" then
					DEBUG.config = msg.config
				end

				if type(msg.decode) == "boolean" then
					DEBUG.decode = msg.decode
				end

				if type(msg.encode) == "boolean" then
					DEBUG.encode = msg.encode
				end

				ub:reply(req, { success = true, msg = "kubed debugging flags updated" })
			end, { fsm = ubus.BOOLEAN, decode = ubus.BOOLEAN, encode = ubus.BOOLEAN }
		},

		deprovision = {
			function(req, msg)
				event:process("e_deprovision", msg.kid, function()
					local reply = string.format("kube with kid=%d has been deprovisioned", msg.kid)
					ub:reply(req, { success = true, msg = reply })
				end)
			end, { kid = ubus.INT32 }
		}
	}
}

ub:add(ub_methods)

local ub_events = {
	["flukso.kube.event"] = function(msg)
		if type(msg.event) == "string" then
			event:process(msg.event)
		end
	end,

	["flukso.kube.packet.rx"] = function(msg)
		if type(msg.hex) ~= "string" then return end
		local arg = { bin = unhex(msg.hex), hex = msg.hex, hdr = { }, pld = { } }
		vstruct.unpack(FMT_HEADER, arg.bin, arg.hdr)

		if DEBUG.decode then dbg.vardump(arg) end

		local e
		if arg.hdr.ctl and arg.hdr.ack then --OAM packet
			if arg.hdr.len == 22 then
				e = "e_pair_request"
			elseif arg.hdr.len == 8 then
				e = "e_upgrade_request"
			elseif arg.hdr.len == 4 then
				e = "e_download_request"
			end
		else
			e = "e_packet"
		end

		event:process(e, arg)
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

uloop:run()
