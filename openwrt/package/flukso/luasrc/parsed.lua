#! /usr/bin/env lua

--[[
    
    parsed.lua - The Flukso telegram parsing daemon

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

local dbg   = require "dbg"
local uci   = require "luci.model.uci".cursor()
local nixio = require "nixio"
nixio.fs    = require "nixio.fs"
nixio.util  = require "nixio.util"

local arg   = arg or {} -- needed when this code is not loaded via the interpreter
local DEBUG = (arg[1] == '-d')

local MAX_SENSORS  = tonumber(uci:get("flukso", "main", "max_sensors"))
local MAX_PROV_SENSORS = tonumber(uci:get("flukso", "main", "max_provisioned_sensors"))

--local DEV = "flukso/protocol/samples.d0"
local DEV = "/dev/ttyS0"
local OUT = "/var/run/spid/delta/out"

--nixio.fs.mkfifo(OUT, '644')
local O_RDWR_NONBLOCK = nixio.open_flags('rdwr', 'nonblock')
local fd = nixio.open(OUT, O_RDWR_NONBLOCK)

local OBIS = {
	["1-0:1.8.1*255"] = { sensor=0, derive=false, sign= 1 },
	["1-0:2.8.1*255"] = { sensor=0, derive=false, sign=-1 },
	["1-0:1.8.2*255"] = { sensor=0, derive=false, sign= 1 },
	["1-0:2.8.2*255"] = { sensor=0, derive=false, sign=-1 },

	["1-0:1.7.0*255"] = { sensor=0, derive=true, sign= 1 },
	["1-0:2.7.0*255"] = { sensor=0, derive=true, sign=-1 },

	-- added for compatibility with Landys & Gyr E350 DSMR2.2+
	["0-1:24.2.0*255"] = { sensor=0, derive=false, sign= 1 },
	["0-1:24.2.1*255"] = { sensor=0, derive=false, sign= 1 },

	-- étiquettes du télé-info client
	["BASE"]    = { sensor=0, derive=false, sign= 1, unit="Wh" },
	["HCHC"]    = { sensor=0, derive=false, sign= 1, unit="Wh" },
	["HCHP"]    = { sensor=0, derive=false, sign= 1, unit="Wh" },
	["EJPHN"]   = { sensor=0, derive=false, sign= 1, unit="Wh" },
	["EJPHPM"]  = { sensor=0, derive=false, sign= 1, unit="Wh" },
	["BBRHCJB"] = { sensor=0, derive=false, sign= 1, unit="Wh" },
	["BBRHPJB"] = { sensor=0, derive=false, sign= 1, unit="Wh" },
	["BBRHCJW"] = { sensor=0, derive=false, sign= 1, unit="Wh" },
	["BBRHPJW"] = { sensor=0, derive=false, sign= 1, unit="Wh" },
	["BBRHCJR"] = { sensor=0, derive=false, sign= 1, unit="Wh" },
	["BBRHPJR"] = { sensor=0, derive=false, sign= 1, unit="Wh" },
	-- puissance apparente!
	["PAPP"]    = { sensor=0, derive=true,  sign= 1, unit="W"  }
}

local FACTOR = {
	W = 1,
	kW = 1000,
	Wh = 1,
	kWh = 1000,
	m3 = 1000,
}

local function get_protocol()
	for i = MAX_SENSORS + 1, MAX_PROV_SENSORS do
		-- stop at first non-provisioned sensor
		if not uci:get("flukso", tostring(i), "enable") then
			break
		end

		local protocol = uci:get("flukso", tostring(i), "protocol")

		if protocol then
			return protocol
		end
	end

	return nil
end

local function map_obis()
	for i = MAX_SENSORS + 1, MAX_PROV_SENSORS do
		-- stop at first non-provisioned sensor
		if not uci:get("flukso", tostring(i), "enable") then
			break
		end

		local obis = uci:get("flukso", tostring(i), "obis")

		if obis then
			for j = 1, #obis do
				if OBIS[obis[j]] then
					OBIS[obis[j]].sensor = i
				end
			end
		end
	end
end

local function msecs()
	local secs, usecs = nixio.gettimeofday()
	secs = secs % 1e6 -- wrap around at 99999 secs
	return secs * 1000 + math.floor(usecs / 1000)
end

local protocol = get_protocol() or "dlms"
local decoder = require ("flukso.decoder." .. protocol)

map_obis()
local get_telegram = decoder.init(DEV)

while true do
	local sensor = {}
	local telegram = assert(get_telegram(), "decoder returned an empty telegram")
	if DEBUG then dbg.vardump(telegram) end

	-- do not process corrupted telegrams
	if telegram.check then
		for obis, map in pairs(OBIS) do
			if telegram[obis] then
				local value, unit

				if protocol == "dlms" then
					value, unit = telegram[obis]:match("^%(([%d%.]+)%*([%w]+)%)$")
				elseif protocol == "tic" then
					value, unit = telegram[obis], map.unit
				end

				-- adapt to delta/out fifo message format
				if value then
					local fvalue = value * FACTOR[unit]

					if not sensor[map.sensor] then
						sensor[map.sensor] = { map.sensor - 1, 0, nil }
					end

					if map.derive then
						sensor[map.sensor][3] = (sensor[map.sensor][3] or 0) + map.sign*fvalue
					else
						sensor[map.sensor][2] = sensor[map.sensor][2] + map.sign*fvalue
					end
				end
			end
		end

		local msg = { os.time() }

		for _key, entry in pairs(sensor) do
			if not entry[3] then
				-- we're dealing with a 'pulse' like sensor, so add the msec timestamp
				entry[3] = msecs()
			end

			msg[#msg + 1] = table.concat(entry, " ")
		end

		msg = table.concat(msg, " ") .. "\n"
		if DEBUG then print(msg) end
		fd:write(msg)
	end
end
