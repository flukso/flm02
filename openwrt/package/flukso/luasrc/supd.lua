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

local ath_kmod_reload   = uci:get('system', 'event', 'ath_kmod_reload')

nixio.fs.mkfifo(FIFO_PATH, '644')
local fifo = nixio.open(FIFO_PATH, O_RDWR)

local disco = {
	buffer = {0, 0, 0, 0},

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
	end
}

for line in fifo:linesource() do
	local timestamp, topic, event = line:match('^(%d+)%s+(%w+)%s+(%w+)$')
	timestamp = tonumber(timestamp)
	nixio.syslog('info', string.format('Received event %s for %s', event, topic))

	if DEBUG then
		print(timestamp, topic, event)
	end

	if event == 'CONNECTED' then
		disco:flush()
	elseif event == 'DISCONNECTED' then
		disco:push(timestamp)

		if disco:glitch() then
			ath_kmod_reload = ath_kmod_reload + 1
			uci:set('system', 'event', 'ath_kmod_reload', ath_kmod_reload)
			uci:commit('system')

			nixio.syslog('alert', 'Too many disconnects on itf ath0, reloading ath kmod')

			os.execute('wifi down')
			os.execute('rmmod ath_ahb')
			os.execute('rmmod ath_hal')
			os.execute('insmod ath_hal')
			os.execute('insmod ath_ahb')
			os.execute('/etc/init.d/network restart')
			disco:flush()
		end
	end

	if DEBUG then
		dbg.vardump(disco.buffer)
	end
end
