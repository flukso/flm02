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

local state, trans, conn = rfsm.state, rfsm.trans, rfsm.conn

local UART_DEV = "/dev/ttyS0"
local UART_BUFFER_SIZE = 4096
local UART_SYNC_PATTERN = "\170\170\170\170" -- 0xaaaaaaaa
local UART_FMT_SEQ_LENGTH = "> @%d seq:u2 length:u2" -- big endian!
local UART_FMT_ADLER32 = "> @%d adler32:u4"
local O_RDWR_NONBLOCK = nixio.open_flags("rdwr", "nonblock")

local DEBUG = {
	packet = false
}

-- define gettime function for rFSM
local function rfsm_gettime()
	local secs, usecs = nixio.gettimeofday()
	return secs, usecs * 1e3
end

rfsm.timeevent.set_gettime_hook(rfsm_gettime)

-- rFSM event parameters
local e_arg

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

	tmp = state {
		entry = function()
		end
	},

	trans { src = "initial", tgt = "tmp" }
}
--[[
local fsm = rfsm.init(root)
rfsm.run(fsm)
]]--
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

	debug = function(self, ...)
		if DEBUG.packet then
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
		if not self.buffer then return self:debug(false) end
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

		return self:debug(true)
	end,
}

uart:flush()
local ufd = uloop.fd(uart:fileno(), uloop.READ, function(events)
		uart:read()
		while uart:packet() do
		end
	end)

uloop:run()
