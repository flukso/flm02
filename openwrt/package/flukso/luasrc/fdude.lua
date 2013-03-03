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
local uci   = require 'luci.model.uci'.cursor()

local SPI_DEV             = '/dev/spidev0.0'
local SPI_DAEMON_PID_FILE = '/var/run/spid/pid'
local O_RDWR_NONBLOCK     = nixio.open_flags('rdwr', 'nonblock')
local MODEL               = 'FLM02X'
uci:foreach('system', 'system', function(x) MODEL = x.model end)
local AVR_DIR        = arg[1] or '/usr/bin/avr'

local spidev = nixio.open(SPI_DEV, O_RDWR_NONBLOCK)
local exit = 0

local function avrdude_cmd()
	local opt = {'', '-p atmega168p'}

	if MODEL == 'FLM02A' then
		opt[#opt+1] = '-c flm02a'
		opt[#opt+1] = '-U lfuse:w:0xEC:m'
		opt[#opt+1] = '-U hfuse:w:0xDE:m'
		opt[#opt+1] = '-U efuse:w:0x01:m'
		opt[#opt+1] = '-U flash:w:' .. AVR_DIR .. '/a/main.hex'
		opt[#opt+1] = '-U eeprom:w:' .. AVR_DIR .. '/a/main.eep'
		
	elseif MODEL == 'FLM02B' then
		opt[#opt+1] = '-c flm02b'
		opt[#opt+1] = '-U lfuse:w:0x6E:m'
		opt[#opt+1] = '-U hfuse:w:0xDE:m'
		opt[#opt+1] = '-U efuse:w:0x01:m'
		opt[#opt+1] = '-U flash:w:' .. AVR_DIR .. '/b/main.hex'
		opt[#opt+1] = '-U eeprom:w:' .. AVR_DIR .. '/b/main.eep'
	else
		print('Unrecognised Fluksometer model')
		exit = 3
		return ''
	end

	return '/usr/bin/avrdude' .. table.concat(opt, ' ')
end

if spidev:lock('tlock') then
	nixio.execp(avrdude_cmd())
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
