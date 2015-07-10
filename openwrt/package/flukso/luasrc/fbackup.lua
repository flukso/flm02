#!/usr/bin/env lua

--[[
    
    fbackup.lua - backup sensor board counters to /etc/config/flukso

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

local nixio = require "nixio"
local uci = require "luci.model.uci".cursor()
local ubus = require "ubus"
local ub = assert(ubus.connect(), "unable to connect to ubus")

-- are we backing up or restoring counters?
local BACKUP = (arg[1] ~= "-r")

-- parse and load /etc/config/flukso
local FLUKSO = uci:get_all("flukso")

local MAX_SENSORS	= tonumber(FLUKSO.main.max_sensors)

-- sensor board commands
local SET_COUNTER	= "sc %d %s"
local GET_COUNTER	= "gc %d"
local GET_COUNTER_R	= "^gc%s+(%d+)%s+(%d+)$"
local COMMIT		= "ct"

--- Convert from Lua-style to c-style index.
-- @param index		Lua-style index startng at 1
-- @return 		C-style index starting at 0 
local function toc(index)
	return index - 1
end

--- Backup counters from the sensor board
-- @param ub		ubus object
-- @param uci		uci cursor object
-- @return		none
local function backup(ub, uci)
	for i = 1, MAX_SENSORS do
		local cmd = string.format(GET_COUNTER, toc(i))
		local reply = ub:call("flukso.flx", "ctrl", { cmd = cmd })

		if reply and reply.result then
			local j, counter = reply.result:match(GET_COUNTER_R)

			uci:set("flukso", i, "counter", counter)
		else
			nixio.syslog("err", string.format("backup failed at sensor %d", i))
			os.exit(1)
		end
	end

	uci:commit("flukso")
	nixio.syslog("info", "backup of counters from sensor board succeeded")
end

--- Restore counters to the sensor board
-- @param ub		ubus object
-- @param uci		uci cursor object
-- @return		none
local function restore(ub, uci)
	for i = 1, MAX_SENSORS do
		local counter = uci:get("flukso", i, "counter")
		local cmd = string.format(SET_COUNTER, toc(i), counter)
		local reply = ub:call("flukso.flx", "ctrl", { cmd = cmd })

		if not reply then
			nixio.syslog("err", string.format("restore failed at sensor %d", i))
			os.exit(2)
		end
	end

	nixio.syslog("info", "restore of counters to sensor board succeeded")
end


nixio.openlog("fbackup", "pid")

if BACKUP then
	backup(ub, uci)
else
	restore(ub, uci)
end
