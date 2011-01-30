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
local data  = require 'flukso.data'

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
local WAN_ENABLED	= true
local WAN_INTERVAL	= 300
local LAN_ENABLED	= true
local TIMESTAMP_MIN	= 1234567890

function dispatch(wan_child, lan_child)
	return coroutine.create(function()
		local delta = { fdin  = nixio.open(DELTA_PATH_IN, O_RDWR_NONBLOCK),
                                fdout = nixio.open(DELTA_PATH_OUT, O_RDWR) }

		if delta.fdin == nil or delta.fdout == nil then
			-- TODO output to syslog
			print('Error. Unable to open the delta fifos.')
			print('Exiting...')
			os.exit(1)
		end

		-- TODO acquire an exclusive lock on the delta fifos or exit

		local function tolua(num)
			return num + 1
		end

		for line in delta.fdout:linesource() do
			print(line)

			local timestamp, data = line:match('^(%d+)%s+([%d%s]+)$')
			timestamp = tonumber(timestamp)

			for i, counter, extra in data:gmatch('(%d+)%s+(%d+)%s+(%d+)') do
				i = tonumber(i)
				counter = tonumber(counter)
				extra = tonumber(extra)

				-- map index(+1!) to sensor id and sensor type
				local sensor_id = FLUKSO[tostring(tolua(i))]['id']
				local sensor_type = FLUKSO[tostring(tolua(i))]['type']

				-- resume both branches
				if WAN_ENABLED then
					coroutine.resume(wan_child, sensor_id, timestamp, counter)
				end

				if LAN_ENABLED then
					if sensor_type == 'analog' then
						coroutine.resume(lan_child, sensor_id, timestamp, extra)

					elseif sensor_type == 'pulse' then
						coroutine.resume(lan_child, sensor_id, timestamp, nil, counter, extra)
					end
				end
				-- check in the e branch whether the counter has increased, if not then discard
				-- chech in both branches whether timestamp has increased
				-- or do we override??
			end 
		end
	end)
end

function wan_buffer(child)
	return coroutine.create(function(sensor_id, timestamp, counter)
		local measurements = data.new()
		local threshold = timestamp + WAN_INTERVAL
		local previous = {}

		while true do
			if not previous[sensor_id] then
				previous[sensor_id] = {}
			end

			if timestamp > TIMESTAMP_MIN
				and timestamp > (previous[sensor_id].timestamp or 0)
				and counter > (previous[sensor_id].counter or 0) 
				then

				measurements:add(sensor_id, timestamp, counter)
				previous[sensor_id].timestamp = timestamp
				previous[sensor_id].counter = counter
			end

			if timestamp > threshold and next(measurements) then  --checking whether table is not empty
				coroutine.resume(child, measurements)
				threshold = timestamp + WAN_INTERVAL
			end

			sensor_id, timestamp, counter = coroutine.yield()
		end
	end)
end

function lan_buffer(child)
	return coroutine.create(function(sensor_id, timestamp, power, counter, msec)
		local measurements = data.new()
		local previous = {}

		local function diff(x, y)  -- calculates y - x
			if y >= x then
				return y - x
			else -- y wrapped around 32-bit boundary
				return 4294967296 - x + y
			end
		end

		while true do
			if not previous[sensor_id] then
				previous[sensor_id] = {}
			end

			if timestamp > TIMESTAMP_MIN and timestamp > (previous[sensor_id].timestamp or 0) then
				if not power then  -- we're dealing pulse message so first calculate power
					if previous[sensor_id].msec and msec > prev[sensor_id].msec then
						power = math.floor(diff(previous[sensor_id].counter, counter) /
                        	                                   diff(previous[sensor_id].msec, msec) * 3.6 * 10^6 + 0.5)
					end

					-- if msec decreased, just update the value in the table
					-- but don't make any power calculations since the AVR might have gone through a reset
					previous[sensor_id].msec = msec
					previous[sensor_id].counter = counter
				end

				measurements:add(sensor_id, timestamp, power)
				previous[sensor_id].timestamp = timestamp
			end

			if next(measurements) then  --checking whether table is not empty
				coroutine.resume(child, measurements)
			end

			sensor_id, timestamp, power, counter, msec = coroutine.yield()
		end
	end)
end

function debug(child)
	return coroutine.create(function(measurements)
		while true do
			if DEBUG then
				dbg.vardump(measurements)
			end

			if child then
				coroutine.resume(child, measurements)
			end

			measurements = coroutine.yield()
		end
	end)
end

local wan_chain =
	wan_buffer(
		debug(nil)
	)

local lan_chain =
	lan_buffer(
		debug(nil)
	)

local chain = dispatch(wan_chain, lan_chain)

coroutine.resume(chain)
