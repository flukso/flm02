#!/usr/bin/env lua

--[[
    
    restful.lua - CGI script providing a local RESTful API on the Fluksometer.

    Copyright (c) 2010 Bart Van Der Meerssche <bart.vandermeerssche@flukso.net>

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


--- Decode a URL's query string
-- @param QS		the to-be-decoded query string (optional)
-- @return		table containing [name] = value pairs
local function query_decode(QS)
	local param = {}

	for name, value in (QS or os.getenv("QUERY_STRING")):gmatch("([^&=]+)=([^&=]+)") do
		param[name] = value
	end

	return param
end

--- Fetch the sensor id in the HTTP request.
-- @return              sensor id
local function sensor_id() 
	return os.getenv("SCRIPT_NAME"):match("/sensor/([%x]+)")
end


local param   = query_decode()
local path    = "/var/run/fluksod/sensor/"
local version = "1.0"

-- Hardcoding path and version parameters lowers GET response time from 200ms to 90ms.
-- local uci     = require "luci.model.uci".cursor()
-- local path    = uci:get("flukso", "main", "localDir") .. "/"
-- local version = uci:get("flukso", "main", "localVersion")

if param.interval == "minute" and (param.unit == "watt" or param.unit == "lperday") and param.version == version then
	local pre, post =  "", ""

	-- jsonp_callback is deprecated but we'll support it for the time being	
	if param.callback or param.jsonp_callback then
		pre, post =  (param.callback or param.jsonp_callback) .. "(", ");"
	end

	io.write("Content-Type: application/json", "\n\n")                                              

	-- if fluksod is not yet generating real-time readings for the sensor
	-- then output an empty array
	if pcall(io.input, path .. sensor_id()) then
		io.write(pre, io.read("*all"), post)
	else
		io.write("[]")
	end
else
	io.write("status: 400 Bad Request", "\n\n")
	io.write("Malformed query string: interval, unit and version query parameters are required.")
end
