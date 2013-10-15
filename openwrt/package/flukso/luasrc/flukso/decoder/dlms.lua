#! /usr/bin/env lua

--[[
    
    dlms.lua - Flukso DLMS/D0 decoder

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


-- DLMS, aka D0, is a line-based, plaintext protocol encapsulating COSEM
-- objects. The first attribute of a COSEM object is an OBIS code defining
-- the object. An OBIS code has a standard A-B:C.D.E*F format.
-- Since value group F is not used in NTA 8130 and its inclusion is optional,
-- we make group F optional and default to 255.
--
-- DLMS has been standardized in IEC 62056-21
-- Abstract, electricity and gas OBIS codes can be obtained at [1]
--
-- [1] http://www.dlms.com/documentation/listofstandardobiscodesandmaintenanceproces/index.html

local dbg = require "dbg"
local nixio = require "nixio"
require "nixio.fs"
require "nixio.util"

local string = string
local coroutine = coroutine

local getfenv, setmetatable, tonumber, tostring, pairs =
      getfenv, setmetatable, tonumber, tostring, pairs

module (...)
local modenv = getfenv()

--[[
Parsed D0 telegram example:

(table) 
	[1-0:1.7.0*255] = (string) (0000.77*kW)
	[0-1:96.1.0*255] = (string) (3338303034303031313338323831383131)
	[1-0:1.8.1*255] = (string) (00212.981*kWh)
	[1-0:1.8.2*255] = (string) (00213.152*kWh)
	[1-0:2.8.1*255] = (string) (00000.000*kWh)
	[0-0:96.1.1*255] = (string) (5A424556303035303735353430393131)
	[0-0:96.3.10*255] = (string) (1)
	[0-1:24.4.0*255] = (string) (1)
	[0-0:96.14.0*255] = (string) (0002)
	[0-1:24.3.0*255] = (string) (120515210000)(00)(60)(1)(0-1:24.2.1)(m3)(00132.534)
	[1-0:2.8.2*255] = (string) (00000.000*kWh)
	[mfg] = (string) ISk
	[0-1:24.1.0*255] = (string) (3)
	[version] = (number) 5
	[model] = (string) \2MT382-1003
	[1-0:2.7.0*255] = (string) (0000.00*kW)
	[0-0:96.13.0*255] = (string) ()
	[0-0:96.13.1*255] = (string) ()
	[0-0:17.0.0*255] = (string) (0999.00*kW)
]]--

local function object_head(A, B, C, D, E, F, data)
	local obis_tpl = "%s-%s:%s.%s.%s*%s"
	local code = obis_tpl:format(A, B, C, D, E, tonumber(F) or 255)

	if telegram then
		telegram[code] = data
		last = code
	end
end

local function object_tail(data)
	if telegram and last then
		telegram[last] = telegram[last] .. data
	end
end

local function start(mfg, version, model)
	version = tonumber(version)

	-- fresh table for storing the datagram's COSEM objects
	telegram = {
		mfg = mfg,
		version = version,
		model = model,
		length = #mfg + #tostring(version) + #model,
		check = true
	}
end

local function finish()
	if telegram then
		if length then
			if telegram.length == length and telegram.check == true then
				sync = true
			else
				telegram.check = false

				-- the first telegram might have been corrupted
				if not sync then
					length = telegram.length
				end				
			end
		else
			length = telegram.length
		end

		local mbus_profile_generic = "^0%-%d+:24%.3%.%d+%*?%d*$"
		local mbus_object = "^%b()%b()%b()%b()%((%d+)%-(%d+):(%d+)%.(%d+)%.(%d+)%*?(%d*)%)%((.+)%)%(([%d%.]+)%)$"
		local cosem_value_tpl = "(%s*%s)"

		for obis, value in pairs(telegram) do
			if obis:find(mbus_profile_generic) then
				local A, B, C, D, E, F, unit, qty = value:match(mbus_object)

				if A then
					object_head(A, B, C, D, E, F, cosem_value_tpl:format(qty, unit))
				end
			end
		end
 
		coroutine.yield(telegram)
		telegram = nil
	end
end

local COSEM = {
	["^(%d+)%-(%d+):(%d+)%.(%d+)%.(%d+)%*?(%d*)(.+)"] = object_head,
	["^(%b())"] = object_tail,
	["^/(%w%w%w)(%d)(.+)"] = start,
	-- dummy capture added to have a non-nil first parameter
	-- TODO add crc16 check when present
	["^(!)"] = finish
}

local function decode(line)
	local match = false

	for pattern, process in pairs(COSEM) do
		local A, B, C, D, E, F, data = line:match(pattern)

		if A then
			process(A, B, C, D, E, F, data)
			match = true
		end
	end

	if telegram then
		telegram.length = telegram.length + #line

		-- not being able to match a specific line
		-- means we're dealing with a corrupted telegram
		if not match and #line > 0 then
			telegram.check = false
		end
	end
end

function init(dev)
	local O_RDWR = nixio.open_flags("rdwr")

	return coroutine.wrap(
		function()
			local fd = nixio.open(dev, O_RDWR_NONBLOCK)
			local telegram = nil
			local last = nil
			local length = nil
			local sync = false

			for line in fd:linesource() do
				decode(line)
			end
		end)
end
