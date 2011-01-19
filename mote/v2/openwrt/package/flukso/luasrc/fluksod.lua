#! /usr/bin/env lua

--[[
    
    fluksod.lua - Lua part of fluksod

    Copyright (C) 2011 Bart Van Der Meerssche <bart.vandermeerssche@flukso.net>

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

require 'dbg'
require 'nixio'
require 'nixio.fs'
local spi = require 'flukso.spi'

DAEMON      = os.getenv('DAEMON') or 'fluksod'
DAEMON_PATH = os.getenv('DAEMON_PATH') or '/var/run/' .. DAEMON

O_RDWR_NONBLOCK = nixio.open_flags('rdwr', 'nonblock')
POLLIN          = nixio.poll_flags('in')

function mkfifos(input)
	local path = string.format('%s/%s/', DAEMON_PATH, input) 

	nixio.fs.mkdirr(path)
	nixio.fs.unlink(path .. 'in')  --> clean up mess from previous run
	nixio.fs.unlink(path .. 'out') --> idem
	nixio.fs.mkfifo(path .. 'in', '644')
	nixio.fs.mkfifo(path .. 'out', '644')

	local fdin  = nixio.open(path .. 'in', O_RDWR_NONBLOCK)
	local fdout = nixio.open(path .. 'out', O_RDWR_NONBLOCK)

	return { fd      = fdin, --> need this entry for nixio.poll
                 fdin    = fdin,
                 fdout   = fdout,
                 events  = POLLIN,
                 revents = 0,
                 line    = fdin:linesource() }
end

local uart  = mkfifos('uart')
local ctrl  = mkfifos('ctrl')
local delta = mkfifos('delta')

local fds = { uart, ctrl, delta }

local spidev = nixio.open('/dev/spidev0.0', O_RDWR_NONBLOCK)

while true do
	if nixio.poll(fds, -1) then
		if delta.revents == POLLIN then
			--> TODO flush the delta fd after each POLLIN
			local msg = spi.new_msg('delta', delta.line())
			msg:parse()
			msg:encode()
			msg:tx(spidev)
			nixio.nanosleep(0, 10e7) --> 10ms
			msg:rx(spidev)
			msg:decode()
			--> dbg.vardump(msg)
			delta.fdout:write(msg.decoded.cmd .. ' ' .. table.concat(msg.decoded, ' ') .. '\n')
		end
	end
end
