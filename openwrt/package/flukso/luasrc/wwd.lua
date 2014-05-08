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

local state, trans, conn, yield = rfsm.state, rfsm.trans, rfsm.conn, rfsm.yield

local DEBUG = {
	decode = false,
	encode = false
}

local ULOOP_TIMEOUT_MS = 1e3
local O_RDWR_NONBLOCK = nixio.open_flags("rdwr", "nonblock")
local UART_DEV = "/dev/ttyS0"
local UART_BUFFER_SIZE = 4096
local UART_SYNC_PATTERN = "\170\170\170\170" -- 0xaaaaaaaa
local UART_FMT_SEQ_LENGTH = "> @%d seq:u2 length:u2" -- big endian!
local UART_FMT_ADLER32 = "> @%d adler32:u4"
local UART_FMT_TLV = "> @%d typ:u2 length:u2"
local UART_FMT_PACKET = "> sync:s seq:u2 length:u2 t:u2 l:u2 v:s"

local UART_RX_EVENT = {
	[0] = "e_rx_ping",
	[1] = "e_rx_basic_usage_data",
	[2] = "e_rx_technical_usage_data",
	[3] = "e_rx_power_consumption_data",
	[9] = "e_rx_error_warning_messages",
--	[10] = "e_rx_subscription_request",
--	[11] = "e_rx_version_info_request",
	[12] = "e_rx_version_info",
--	[13] = "e_rx_statistics_info_request",
	[14] = "e_rx_statistics_info",
--	[15] = "e_rx_config_info_request",
	[16] = "e_rx_battery type data",
	[17] = "e_rx_generator_type_data",
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
			return tlv.typ, tlv.length, tlv.value
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

	receiving = state {
		entry = function()
		end
	},

	ping = state {
		entry = function()
			s_ctx = e_arg -- store cb function in ctx
			uart:write("ping")
		end
	},

	pong = state {
		entry = function()
			uart:write("pong")
		end
	},

	trans { src = "initial", tgt = "receiving" },
	trans { src = "receiving", tgt = "ping", events = { "e_tx_ping" } },
	trans { src = "ping", tgt = "receiving", events = { "e_rx_pong" },
		effect = function() s_ctx(true) end
	},
	trans { src = "ping", tgt = "receiving", events = { "e_after(1)" },
		effect = function() s_ctx(false) end
	},
	trans { src = "receiving", tgt = "pong", events = { "e_rx_ping" } },
	trans { src = "pong", tgt = "receiving", events = { "e_done" } },
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
			end, { fsm = ubus.BOOLEAN, decode = ubus.BOOLEAN }
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
	end, ULOOP_TIMEOUT_MS)

uart:flush()
local ufd = uloop.fd(uart:fileno(), uloop.READ, function(events)
		uart:read()
		while uart:packet() do
			for typ, length, value in uart:tlv() do
				event:process(UART_RX_EVENT[typ], value)
			end
		end
	end)

uloop:run()
