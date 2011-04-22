--[[
    
    sensor.lua - LuCI cbi model for the Flukso sensor page

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


local uci = require "luci.model.uci".cursor()

local FLUKSO      = uci:get_all("flukso")
local MAX_SENSORS = tonumber(FLUKSO.main.max_sensors)


m = Map("flukso", translate("Sensor"), translate("Fluksometer sensor configuration"))

s = m:section(NamedSection, "main", "settings", "general settings")
phase = s:option(ListValue, "phase", translate("phase"))
phase:value("1")
phase:value("3")

-- (ab)use phase validation callback to assgn port numbers to sensors
function phase:validate(value, section)
	if value == "1" then
		uci:set_list("flukso", "1", "port", "1")
		uci:set_list("flukso", "2", "port", "2")
		uci:set_list("flukso", "3", "port", "3")
	elseif value == "3" then
		uci:set_list("flukso", "1", "port", { "1", "2", "3" })
		uci:delete  ("flukso", "2", "port")
		uci:delete  ("flukso", "3", "port")

		uci:set("flukso", "2", "enable", "0")
		uci:set("flukso", "3", "enable", "0")
	end

	uci:commit("flukso")

	return value
end

for i = 1, MAX_SENSORS do
	s = m:section(NamedSection, tostring(i), "sensor", "sensor #" .. i)
	s.addremove = false

	enable = s:option(Flag, "enable", translate("enable"))
	enable.rmempty = false

	s:option(DummyValue, "id", translate("identifier"))
	s:option(DummyValue, "class", translate("class"))

	if FLUKSO[tostring(i)].class == "analog" then
		typ = s:option(DummyValue, "type", translate("type"))

		func = s:option(Value, "function", translate("function"))
		func:depends("enable", "1")
		func.rmempty = true

		voltage = s:option(Value, "voltage", translate("voltage"))
		voltage.rmempty = false

		current = s:option(ListValue, "current", translate("current"))
		current:value("50")
		current:value("100")
		current:value("250")
		current:value("500")
		current.rmempty = false
	
	elseif FLUKSO[tostring(i)].class == "pulse" then
		typ = s:option(Value, "type", translate("type"))
		typ.rmempty = true

		func = s:option(Value, "function", translate("function"))
		func:depends("enable", "1")
		func.rmempty = true

		constant = s:option(Value, "constant", translate("constant"))
		constant.rmempty = false
	end
end


-- section-level validation does not seem to work with NamedSections
-- so we're resorting to hackery page-level validation instead
function m:parse(...)
	-- insert custom processing

	for _, sobj in ipairs(self.children) do
		local sid = sobj.section

		-- only process the 'sensor' sections
		if tonumber(sid) then
			local err = {}
			local field = {}

			for _, fld in ipairs(sobj.children) do		
				field[fld.option] = fld
			end

			-- an enabled sensor requires a non-empty function/name
			if field["class"]:cfgvalue(sid) ~= "uart"
				and field["enable"]:formvalue(sid) == "1"
				and field["function"]:formvalue(sid) == "" then

				err[#err + 1] = "an enabled sensor must have a name"
			end	

			-- an enabled sensor requires a non-empty type
			if field["class"]:cfgvalue(sid) == "pulse"
				and field["enable"]:formvalue(sid) == "1"
				and field["type"]:formvalue(sid) == "" then

				err[#err + 1] = "an enabled sensor must have a type"
			end

			if next(err) then
				self.save = false

				field["enable"].section.error = {
					[sid] = err
				}
			end
		end
	end

	-- and now call the authentic parser
	Map.parse(self, ...)
end

-- sync with the sensor board after committing changes to the uci file
m.on_after_commit = function(self)
	luci.util.exec("fsync")
end


return m
