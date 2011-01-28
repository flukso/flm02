#!/usr/bin/env lua

--[[
    
    fsync.lua - synchronize /etc/config/flukso settings with the sensor board
                via the fluksod ctrl fifos

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

local CTRL_PATH		= '/var/run/fluksod/ctrl'
local CTRL_PATH_IN	= CTRL_PATH .. '/in'
local CTRL_PATH_OUT	= CTRL_PATH .. '/out'

local O_RDWR_NONBLOCK	= nixio.open_flags('rdwr', 'nonblock')
local POLLIN		= nixio.poll_flags('in')
local POLL_TIMEOUT_MS	= 1000
local MAX_TRIES         = 5

local ctrl = { fdin    = nixio.open(CTRL_PATH_IN, O_RDWR_NONBLOCK),
               fdout   = nixio.open(CTRL_PATH_OUT, O_RDWR_NONBLOCK),
               events  = POLLIN,
               revents = 0 }

ctrl.fd = ctrl.fdout -- need this entry for nixio.poll
ctrl.line = ctrl.fdout:linesource()

if ctrl.fdin == nil or ctrl.fdout == nil then
	print('Unable to open the ctrl fifos.')
	print('Exiting...')
	os.exit(1)
end

-- TODO acquire an exclusive lock on the ctrl fifos or exit


local function send(ctrl, cmd)
	while ctrl.line() do end -- flush the out fifo

	for i = 1, MAX_TRIES do
		ctrl.fdin:write(cmd .. '\n')

		local poll, errno, errmsg = nixio.poll({ ctrl }, POLL_TIMEOUT_MS)

		if poll < 0 then
			print('Poll failed with error message: ' .. errmsg)

		elseif poll == 0 then
			print('Poll timed out after ' .. POLL_TIMEOUT_MS .. 'ms')

		elseif poll > 0 then
			reply = ctrl.line()

			if cmd:sub(1, 1) == 's' then
				if reply == cmd then
					print(reply .. ' .. ok')
					return reply
				else
					print(reply .. ' .. nok .. should be ' .. cmd .. ' instead')
				end
			elseif cmd:sub(1, 2) == reply:sub(1, 2) then
				print(reply .. ' .. ok')
				return reply
			else
				print(reply .. ' .. nok')
			end	
		end
	end

	print(MAX_TRIES .. ' write attempts failed. Exiting ...')
	os.exit(2) 
end

local function toc(num)
	return num - 1
end


-- parse and load /etc/config/flukso
local flukso = uci:get_all('flukso')

local HW_CHECK_OVERRIDE = (arg[1] == '-f')

local MAX_SENSORS	= tonumber(flukso.main.max_sensors)
local METERCONST_FACTOR	= 0.449

local GET_HW_VERSION	= 'gh'
local GET_HW_VERSION_R	= '^gh%s+(%d+)%s+(%d+)$'
local SET_ENABLE	= 'se %d %d'
local SET_PHY_TO_LOG	= 'sp' -- with [1..MAX_SENSORS] arguments
local SET_METERCONST	= 'sm %d %d'
local SET_COUNTER	= 'sc %d %d'

-- check hardware version
local hw_major, hw_minor = send(ctrl, GET_HW_VERSION):match(GET_HW_VERSION_R)

if hw_major ~= flukso.main.hw_major or hw_minor > flukso.main.hw_minor then
	print(string.format('Hardware check (major: %s, minor: %s) .. nok', hw_major, hw_minor))
	if hw_major ~= flukso.main.hw_major then
		print('Major version does not match.')
	end

	if hw_minor > flukso.main.hw_minor then
		print('Sensor board minor version is not supported by this package.')
	end

	if HW_CHECK_OVERRIDE then
		print('Overridden. Good luck!')
	else
		print('Use -f to override this check at your own peril.')
		os.exit(3)
	end
else
	print(string.format('Hardware check (major: %s, minor: %s) .. ok', hw_major, hw_minor))
end

-- disable all ports
for i = 1, MAX_SENSORS do
	local cmd = string.format(SET_ENABLE, toc(i), 0)
	send(ctrl, cmd)
end

-- populate phy_to_log
local phy_to_log = {}

for i = 1, MAX_SENSORS do
	local set = { analog = true, pulse = true }

	if flukso[tostring(i)] ~= nil and set[flukso[tostring(i)]['type']] then
		local ports = flukso[tostring(i)].port or {}

		for j = 1, #ports do
			phy_to_log[toc(tonumber(ports[j]))] = toc(i)
		end
	end
end

for i = 0, MAX_SENSORS - 1 do
	if not phy_to_log[i] then
		phy_to_log[i] = 0xff
	end
end

local cmd = SET_PHY_TO_LOG .. ' ' .. table.concat(phy_to_log, ' ', 0)
send(ctrl, cmd)

-- populate meterconst
for i = 1, MAX_SENSORS do
	local cmd

	if flukso[tostring(i)] == nil then
		cmd = string.format(SET_METERCONST, toc(i), 0)

	elseif flukso[tostring(i)]['type'] == 'analog' then
		local voltage = tonumber(flukso[tostring(i)].voltage)
		local current = tonumber(flukso[tostring(i)].current)
		cmd = string.format(SET_METERCONST, toc(i), math.floor(METERCONST_FACTOR * voltage * current))

	elseif flukso[tostring(i)]['type'] == 'pulse'then
		local meterconst = tonumber(flukso[tostring(i)].constant)
		cmd = string.format(SET_METERCONST, toc(i), meterconst)
	else
		cmd = string.format(SET_METERCONST, toc(i), 0)
	end

	if cmd then send(ctrl,cmd) end
end

-- populate counter if reset_counters is set
if flukso.main.reset_counters == '1' then
	for i = 1, MAX_SENSORS do
		local cmd = string.format(SET_COUNTER, toc(i), 0)
		send(ctrl, cmd)
	end

	uci:set('flukso', 'main', 'reset_counters', 0)
	uci:commit('flukso')
end

-- enable configured ports
for i = 1, MAX_SENSORS do
	if flukso[tostring(i)] ~= nil then
		local ports = flukso[tostring(i)].port or {}

		for j = 1, #ports do
			cmd = string.format(SET_ENABLE, toc(tonumber(ports[j])), 1)
			send(ctrl, cmd)
		end
	end
end

-- clean up
ctrl.fdin:close()
ctrl.fdout:close()

print(arg[0] .. ' completed successfully. Bye!')
