#! /usr/bin/env lua

--[[
    
    supd.lua - The Flukso Supervisor

    Copyright (C) 2013 Bart Van Der Meerssche <bart@flukso.net>

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

local dbg   = require 'dbg'
local nixio = require 'nixio'
nixio.fs    = require 'nixio.fs'
local luci  = require 'luci'
luci.sys    = require 'luci.sys'
local uci   = require 'luci.model.uci'.cursor()

local arg = arg or {} -- needed when this code is not loaded via the interpreter

local DEBUG             = (arg[1] == '-d')
local LOGMASK           = uci:get('flukso', 'daemon', 'logmask') or 'info'
nixio.setlogmask(LOGMASK)

local DAEMON            = os.getenv('DAEMON') or 'supd'
local DAEMON_PATH 	    = os.getenv('DAEMON_PATH') or '/var/run/' .. DAEMON
local FIFO_PATH         = DAEMON_PATH .. '/event'

local O_RDWR            = nixio.open_flags('rdwr')
local O_RDWR_NONBLOCK   = nixio.open_flags('rdwr', 'nonblock')
local O_RDWR_CREAT	    = nixio.open_flags('rdwr', 'creat')

local POLLIN            = nixio.poll_flags('in')
local POLL_TIMEOUT_MS   = -1 -- never

local TIMERFD_SEC       = 300
local TIMERFD_NS        = 0

nixio.fs.mkfifo(FIFO_PATH, '644')
local fdin = nixio.open(FIFO_PATH, O_RDWR_NONBLOCK)
local fifo = {
	fd      = fdin,
	events  = POLLIN,
	revents = 0,
	line    = fdin:linesource()
}

local timer = {
	fd      = nixio.timerfd(TIMERFD_SEC, TIMERFD_NS, TIMERFD_SEC, TIMERFD_NS),
	events  = POLLIN,
	revents = 0
}

local fds = { fifo, timer }

local disco = {
	buffer = {0, 0, 0, 0},
	ath_kmod_reload = uci:get('system', 'event', 'ath_kmod_reload'),

	flush = function(self)
		self.buffer = {0, 0, 0, 0}
	end,

	push = function(self, timestamp)
		table.remove(self.buffer, 1)
		self.buffer[#self.buffer + 1] = timestamp
	end,

	glitch = function(self)
		if self.buffer[#self.buffer] - self.buffer[1] < 60 then
			return true
		end

		return false
	end,

	reload = function(self)
		self.ath_kmod_reload = self.ath_kmod_reload + 1
		uci:set('system', 'event', 'ath_kmod_reload', self.ath_kmod_reload)
		uci:commit('system')

		nixio.syslog('alert', 'Too many disconnects on itf ath0, reloading ath kmod')

		os.execute('wifi down')
		os.execute('rmmod ath_ahb')
		os.execute('rmmod ath_hal')
		os.execute('insmod ath_hal')
		os.execute('insmod ath_ahb')
		os.execute('/etc/init.d/network restart')
	end,

	loop = function(self, event, timestamp)
		if event == 'CONNECTED' then
			self:flush()
		elseif event == 'DISCONNECTED' then
			self:push(timestamp)

			if self:glitch() then
				self:reload()
				self:flush()
			end
		end

		if DEBUG then
			dbg.vardump(self.buffer)
		end
	end
}

local ntp = {
	on = false,

	running = function(self)
		local ps = luci.sys.process.list()
		for k, proc in pairs(ps) do
			if proc.COMMAND:find('ntpclient') then
				self.on = true
				break
			end
		end
	end,

	check = function(self)
		if self.on then
			if DEBUG then
				print('checking ntpclient')
			end

			os.execute('fntp')
		end
	end,

	loop = function(self, event)
		if event == 'START' then
               self.on = true
		elseif event == 'STOP' then
               self.on = false
		end
	end
}

ntp:running()

while true do
	local poll = nixio.poll(fds, POLL_TIMEOUT_MS)

	if not poll then      -- poll == -1
	elseif poll == 0 then -- poll timed out
	elseif poll > 0 then
		if fifo.revents == POLLIN then
			local timestamp, topic, event = fifo.line():match('^(%d+)%s+(%w+)%s+(%w+)$')
			timestamp = tonumber(timestamp)
			nixio.syslog('info', string.format('Received event %s for %s', event, topic))

			if DEBUG then
				print(timestamp, topic, event)
			end

			if topic == 'ath0' then
				disco:loop(event, timestamp)
			elseif topic == 'ntp' then
				ntp:loop(event)
			end
		elseif timer.revents == POLLIN then
			timer.fd:numexp() -- reset the numexp counter
			ntp:check()
		end
	end
end
