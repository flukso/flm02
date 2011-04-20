--[[
LuCI - Lua Configuration Interface

Copyright 2008 Steven Barth <steven@midlink.org>
Copyright 2008 Jo-Philipp Wich <xm@leipzig.freifunk.net>
Copyright 2011 Bart Van Der Meerssche <bart.vandermeerssche@flukso.net>

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

	http://www.apache.org/licenses/LICENSE-2.0

$Id: network.lua 5949 2010-03-27 14:56:35Z jow $
]]--

local uci = require "luci.model.uci".cursor()

local FLUKSO      = uci:get_all("flukso")
local MAX_SENSORS = tonumber(FLUKSO.main.max_sensors)


m = Map("flukso", translate("Sensor"), translate("Fluksometer sensor configuration"))

-- sync with the sensor board after committing to the uci file
m.on_after_commit = function(self)
	luci.util.exec("fsync")
end


s = m:section(NamedSection, "main", "settings", "general settings")
phase = s:option(ListValue, "phase", translate("phase"))
phase:value("1")
phase:value("3")

-- (ab)use phase validation callback to assign port numbers to sensors
function phase:validate(value, section)
	if value == "1" then
		uci:set_list("flukso", "1", "port", "1")
		uci:set_list("flukso", "2", "port", "2")
		uci:set_list("flukso", "3", "port", "3")
	elseif value == "3" then
		uci:set_list("flukso", "1", "port", { "1", "2", "3" })
		uci:delete  ("flukso", "2", "port")
		uci:delete  ("flukso", "3", "port")
	end

	uci:commit("flukso")

	return value
end


s = {}

for i = 1, MAX_SENSORS do
	s[i] = m:section(NamedSection, tostring(i), "sensor", "sensor #" .. i)
	s[i].addremove = false

	s[i]:option(DummyValue, "id", translate("identifier"))
	s[i]:option(DummyValue, "class", translate("class"))

	if FLUKSO[tostring(i)].class == "analog" then
		s[i]:option(Value, "voltage", translate("voltage"))
		current = s[i]:option(ListValue, "current", translate("current"))
		current:value("50")
		current:value("100")
		current:value("250")
		current:value("500")
	
	elseif FLUKSO[tostring(i)].class == "pulse" then
		s[i]:option(Value, "constant", translate("constant"))
	end

	enable = s[i]:option(Flag, "enable", translate("enable"))
	enable.rmempty = false
end

return m
