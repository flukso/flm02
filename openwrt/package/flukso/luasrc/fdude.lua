#!/usr/bin/env lua

--[[
    
    fdude.lua - wraps avrdude in an spidev resource lock

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


local nixio = require 'nixio'

local SPI_DEV			= '/dev/spidev0.0'
local SPI_DAEMON_PID_FILE	= '/var/run/spid/pid'
local O_RDWR_NONBLOCK		= nixio.open_flags('rdwr', 'nonblock')

local spidev = nixio.open(SPI_DEV, O_RDWR_NONBLOCK)
local exit = 0

if spidev:lock('tlock') then
	nixio.execp('avrdude', ...)
else
	print(string.format('Detected a lock on %s', SPI_DEV))

	local pid = nixio.open(SPI_DAEMON_PID_FILE, O_RDWR_NONBLOCK)

	if pid then
		print(string.format('spid process is still running with pid %d', pid:read(-1)))
		exit = 1
	else
		print('spid is not running.')
		print('Could be a second avrdude still in progress.')
		exit = 2
	end
	
	print('Aborting...')
end

os.exit(exit)
