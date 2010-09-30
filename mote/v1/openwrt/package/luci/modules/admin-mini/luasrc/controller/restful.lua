--[[
LuCI - Lua Configuration Interface

Copyright 2008 Steven Barth <steven@midlink.org>
Copyright 2008 Jo-Philipp Wich <xm@leipzig.freifunk.net>

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

	http://www.apache.org/licenses/LICENSE-2.0

$Id: rpc.lua 5118 2009-07-23 03:32:30Z jow $
]]--

module ("luci.controller.restful", package.seeall)

function index()
	local uci = require "luci.model.uci".cursor()

	if uci:get("flukso", "main", "localEnable") == "1" then
		uci:foreach("flukso", "sensor", function(section)
			entry({section.id}, call("rest_sensor", {section.id}))
		end)
	end
end

--- Decode a URL query string
-- @param QS		the to-be-decoded query string
-- @return		table containing [name] = value pairs
-- @see			rest_sensor
local function decode(QS)
	local param = {}

	for name, value in QS:gmatch("([^&=]+)=([^&=]+)") do
		param[name] = value
	end

	return param
end

--- Return a 400 error code to the client
-- @param message	custom error message (optional)
-- @return		false
function error400(message)
	local http     = require "luci.http"

	http.status(400, "Bad Request")
	message = message or "Bad Request"

	http.prepare_content("text/plain")
	http.write(message)
	
	return false
end

--- Callback function for Flukso RESTful API
-- @param id		requested sensor id (table)
-- @return		json-encoded time series data 
function rest_sensor(id)
	local http     = require "luci.http"                                                          
	local ltn12    = require "luci.ltn12"                                                         
	local uci      = require "luci.model.uci".cursor()

	local path     = uci:get("flukso", "main", "localDir")
	local param    = decode(http.getenv("QUERY_STRING"))

	if param.interval == "minute" and param.unit == "watt" then
		http.prepare_content("application/json")
		http.write("sensor:" .. (id[1] or 'nok') .. "\n" ..
			   "query string:" .. http.getenv("QUERY_STRING") .. "\n" ..
			   "contents:")

		local source = ltn12.source.file(io.open(path .. "/" .. id[1], "r"))
		ltn12.pump.all(source, http.write)
	else
		error400("Malformed query string: 'interval' and 'unit' query parameters are obligatory.")
	end
end
