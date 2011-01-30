#! /usr/bin/env lua

--[[
    
    fluksod.lua - Lua part of the Flukso daemon

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


local dbg   = require 'dbg'
local nixio = require 'nixio'
nixio.fs    = require 'nixio.fs'
local uci   = require 'luci.model.uci'.cursor()

local arg = arg or {} -- needed when this code is not loaded via the interpreter

local DEBUG		= (arg[1] == '-d')

local DAEMON 		= os.getenv('DAEMON') or 'fluksod'
local DAEMON_PATH 	= os.getenv('DAEMON_PATH') or '/var/run/' .. DAEMON

local DELTA_PATH	= '/var/run/spid/delta'
local DELTA_PATH_IN	= DELTA_PATH .. '/in'
local DELTA_PATH_OUT	= DELTA_PATH .. '/out'

local O_RDWR		= nixio.open_flags('rdwr')
local O_RDWR_NONBLOCK   = nixio.open_flags('rdwr', 'nonblock')
local POLLIN            = nixio.poll_flags('in')

-- parse and load /etc/config/flukso
local FLUKSO		= uci:get_all('flukso')


local delta = { fdin  = nixio.open(DELTA_PATH_IN, O_RDWR_NONBLOCK),
                fdout = nixio.open(DELTA_PATH_OUT, O_RDWR) }

if delta.fdin == nil or delta.fdout == nil then
	-- TODO output to syslog
	print('Error. Unable to open the delta fifos.')
	print('Exiting...')
	os.exit(1)
end

-- TODO acquire an exclusive lock on the delta fifos or exit

function tolua(num)
	return num + 1
end

for line in delta.fdout:linesource() do
	print(line)

	timestamp, data = line:match('^(%d+)%s+([%d%s]+)$')

	for i, counter, extra in data:gmatch('(%d+)%s+(%d+)%s+(%d+)') do

		-- map index(+1!) to sensor id and sensor type
		local sensor_id = FLUKSO[tostring(tolua(i))]['id']
		local sensor_type = FLUKSO[tostring(tolua(i))]['type']

		print(sensor_id, sensor_type, counter, extra)

		-- resume both branches
		-- check in the e branch whether the counter has increased, if not then discard
		-- chech in both branches whether timestamp has increased
		-- or do we override??
	end 
end



