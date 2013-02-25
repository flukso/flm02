#! /usr/bin/env lua

--[[
    
    fluksod.lua - Lua part of the Flukso daemon

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


local dbg        = require 'dbg'
local nixio      = require 'nixio'
nixio.fs         = require 'nixio.fs'
local uci        = require 'luci.model.uci'.cursor()
local httpclient = require 'luci.httpclient'
local data       = require 'flukso.data'
local mosq       = require 'mosquitto'

-- parse and load /etc/config/flukso
local FLUKSO            = uci:get_all('flukso')

local arg = arg or {} -- needed when this code is not loaded via the interpreter

local DEBUG             = (arg[1] == '-d')
local LOGMASK           = FLUKSO.daemon.logmask or 'info'
nixio.setlogmask(LOGMASK)

local DAEMON 		    = os.getenv('DAEMON') or 'fluksod'
local DAEMON_PATH 	    = os.getenv('DAEMON_PATH') or '/var/run/' .. DAEMON

local DELTA_PATH        = '/var/run/spid/delta'
local DELTA_PATH_IN	    = DELTA_PATH .. '/in'
local DELTA_PATH_OUT    = DELTA_PATH .. '/out'

local O_RDWR		    = nixio.open_flags('rdwr')
local O_RDWR_NONBLOCK   = nixio.open_flags('rdwr', 'nonblock')
local O_RDWR_CREAT	    = nixio.open_flags('rdwr', 'creat')

local POLLIN            = nixio.poll_flags('in')

-- set WAN parameters
local WAN_ENABLED       = (FLUKSO.daemon.enable_wan_branch == '1')

local TIMESTAMP_MIN	    = 1234567890
local WAN_INTERVAL	    = 300

local WAN_FILTER        = { [1] = {}, [2] = {}, [3] = {} }
WAN_FILTER[1].span      = 60
WAN_FILTER[1].offset    = 0
WAN_FILTER[2].span      = 900
WAN_FILTER[2].offset    = 7200
WAN_FILTER[3].span      = 86400
WAN_FILTER[3].offset    = 172800

local WAN_BASE_URL      = FLUKSO.daemon.wan_base_url .. 'sensor/'
local WAN_KEY           = '0123456789abcdef0123456789abcdef'
uci:foreach('system', 'system', function(x) WAN_KEY = x.key end) -- quirky but it works

-- https headers
local FLUKSO_VERSION    = '000'
uci:foreach('system', 'system', function(x) FLUKSO_VERSION = x.version end) -- quirky but it works, again

local USER_AGENT        = 'Fluksometer v' .. FLUKSO_VERSION
local CACERT            = FLUKSO.daemon.cacert

-- set LAN parameters
local LAN_ENABLED       = (FLUKSO.daemon.enable_lan_branch == '1')

local LAN_INTERVAL      = 0
local LAN_POLISH_CUTOFF	= 60
local LAN_PUBLISH_PATH  = DAEMON_PATH .. '/sensor'

local LAN_FACTOR = {
	['electricity']     =      3.6e6, -- 1 Wh/ms = 3.6e6 W
	['water']           = 24 * 3.6e6, -- 1 L/ms  = 24 * 3.6e6 L/day
	['gas']             = 24 * 3.6e6  -- 1 L/ms  = 24 * 3.6e6 L/day
}

local LAN_ID_TO_FACTOR  = { }
uci:foreach('flukso', 'sensor', function(x) LAN_ID_TO_FACTOR[x.id] = LAN_FACTOR[x['type']] end)

function resume(...)
	local status, err = coroutine.resume(...)

	if not status then
		error(err, 0)
	end
end

-- mosquitto client params
local MOSQ_ID           = DAEMON
local MOSQ_CLN_SESSION  = true
local MOSQ_HOST         = 'localhost'
local MOSQ_PORT         = 1883
local MOSQ_KEEPALIVE    = 60
local MOSQ_TIMEOUT      = 0 -- return instantly from select call
local MOSQ_MAX_PKTS     = 10 -- packets
local MOSQ_QOS          = 0
local MOSQ_RETAIN       = true

-- connect to the MQTT broker
mosq.init()
local mqtt = mosq.new(MOSQ_ID, MOSQ_CLN_SESSION)
mqtt:connect(MOSQ_HOST, MOSQ_PORT, MOSQ_KEEPALIVE)

function dispatch(wan_child, lan_child)
	return coroutine.create(function()
		local delta = {
			fdin  = nixio.open(DELTA_PATH_IN, O_RDWR_NONBLOCK),
			fdout = nixio.open(DELTA_PATH_OUT, O_RDWR)
		}

		if delta.fdin == nil or delta.fdout == nil then
			nixio.syslog('alert', 'cannot open the delta fifos')
			os.exit(1)
		end

		-- acquire an exclusive lock on the delta fifos or exit
		if not (delta.fdin:lock('tlock') and delta.fdout:lock('tlock')) then
			nixio.syslog('alert', 'detected a lock on the delta fifos')
			os.exit(2)
		end

		local function tolua(num)
			return num + 1
		end

		for line in delta.fdout:linesource() do
			-- service the mosquitto loop
			if not mqtt:loop(MOSQ_TIMEOUT, MOSQ_MAX_PKTS) then
				mqtt:reconnect()
			end

			if DEBUG then
				print(line)
			end

			local timestamp, data = line:match('^(%d+)%s+([%d%s]+)$')
			timestamp = tonumber(timestamp)

			for i, counter, extra in data:gmatch('(%d+)%s+(%d+)%s+(%d+)') do
				i = tonumber(i)
				counter = tonumber(counter)
				extra = tonumber(extra)

				-- map index(+1!) to sensor id and sensor type
				local sensor_id = FLUKSO[tostring(tolua(i))]['id']
				local sensor_class = FLUKSO[tostring(tolua(i))]['class']
				local sensor_derive = (FLUKSO[tostring(tolua(i))]['derive'] == '1')

				-- resume both branches
				if WAN_ENABLED then
					resume(wan_child, sensor_id, timestamp, counter)
				end

				if LAN_ENABLED then
					if sensor_class == 'analog' or (sensor_class == 'cosem' and sensor_derive) then
						resume(lan_child, sensor_id, timestamp, extra)

					elseif sensor_class == 'pulse' or (sensor_class == 'cosem' and not sensor_derive) then
						resume(lan_child, sensor_id, timestamp, false, counter, extra)
					end
				end
			end 
		end
	end)
end

function wan_buffer(child)
	return coroutine.create(function(sensor_id, timestamp, counter)
		local measurements = data.new()
		local threshold = os.time() + WAN_INTERVAL
		local previous = {}

		local topic_fmt = '/sensor/%s/counter'
		local payload_fmt = '[%d,%d]'

		while true do
			if not previous[sensor_id] then
				previous[sensor_id] = {}
				-- use the first received counter value as guard
				previous[sensor_id].timestamp = timestamp
				previous[sensor_id].counter = counter
			end

			if timestamp > TIMESTAMP_MIN
				and timestamp > (previous[sensor_id].timestamp or 0)
				and counter ~= (previous[sensor_id].counter or 0) 
				then

				nixio.syslog('info', string.format('processed pulse %s:%s:%s', sensor_id, timestamp, counter))

				local topic = string.format(topic_fmt, sensor_id)
				local payload = string.format(payload_fmt, timestamp, counter)
				mqtt:publish(topic, payload, MOSQ_QOS, MOSQ_RETAIN)

				measurements:add(sensor_id, timestamp, counter)
				previous[sensor_id].timestamp = timestamp
				previous[sensor_id].counter = counter
			end

			if timestamp > threshold and next(measurements) then  --checking whether table is not empty
				resume(child, measurements)
				threshold = os.time() + WAN_INTERVAL
			end

			sensor_id, timestamp, counter = coroutine.yield()
		end
	end)
end

function filter(child, span, offset)
	return coroutine.create(function(measurements)
		while true do
			measurements:filter(span, offset)
			resume(child, measurements)
			measurements = coroutine.yield()
		end
	end)
end


function send(child)
	return coroutine.create(function(measurements)
		local headers = {}
  		headers['Content-Type'] = 'application/json'
		headers['X-Version'] = '1.0'
		headers['User-Agent'] = USER_AGENT

		local options = {}
		options.sndtimeo = 5
		options.rcvtimeo = 5
		options.method  = 'POST'
		options.tls_context_set_verify = 'peer'
		options.cacert = CACERT
		options.headers = headers

		while true do
			local sensors = measurements:get_sensors()
			local measurements_json = measurements:json_encode()
			local http_persist = httpclient.create_persistent()

			for i, sensor_id in ipairs(sensors) do
				if i ~= #sensors then
					options.headers['Connection'] = 'keep-alive'
				else
					options.headers['Connection'] = 'close'
				end

				options.body = '{"measurements":' .. measurements_json[sensor_id] .. '}'
				options.headers['Content-Length'] = tostring(#options.body)
				
				local hash = nixio.crypto.hmac('sha1', WAN_KEY)
				hash:update(options.body)
				options.headers['X-Digest'] = hash:final()

				local url = WAN_BASE_URL .. sensor_id
				local response, code, call_info = http_persist(url, options)

				local level

				-- flush the sensor's measurement buffer in case of a successful HTTP POST
				if code == 200 then
					measurements:clear(sensor_id)
					level = 'info'
				else
					level = 'err'
				end

				nixio.syslog(level, string.format('%s %s: %s', options.method, url, code))

				-- if available, send additional error info to the syslog
				if type(call_info) == 'string' then
					nixio.syslog('err', call_info)
				elseif type(call_info) == 'table'  then
					local auth_error = call_info.headers['WWW-Authenticate']

					if auth_error then
						nixio.syslog('err', string.format('WWW-Authenticate: %s', auth_error))
					end
				end
			end

			-- allow coroutine to be gc'ed
			http_persist = nil

			resume(child, measurements)
			measurements = coroutine.yield()
		end
	end)
end

function gc(child)
	return coroutine.create(function(measurements)
		while true do
			collectgarbage() -- force a complete garbage collection cycle
			resume(child, measurements)
			measurements = coroutine.yield()
		end
	end)
end

function lan_buffer(child)
	return coroutine.create(function(sensor_id, timestamp, power, counter, msec)
		local measurements = data.new()
		local threshold = os.time() + LAN_INTERVAL
		local previous = {}

		local topic_fmt = '/sensor/%s/flux'
		local payload_fmt = '[%d,%d]'

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
					if previous[sensor_id].msec and msec > previous[sensor_id].msec then
						power = math.floor(
							diff(previous[sensor_id].counter, counter) /
							diff(previous[sensor_id].msec, msec) *
							(LAN_ID_TO_FACTOR[sensor_id] or 1000) +
							0.5)

					end

					-- if msec decreased, just update the value in the table
					-- but don't make any power calculations since the AVR might have gone through a reset
					previous[sensor_id].msec = msec
					previous[sensor_id].counter = counter
				end

				if power then
					local topic = string.format(topic_fmt, sensor_id)
					local payload = string.format(payload_fmt, timestamp, power)
					mqtt:publish(topic, payload, MOSQ_QOS, MOSQ_RETAIN)

					measurements:add(sensor_id, timestamp, power)
					previous[sensor_id].timestamp = timestamp
				end
			end

			if timestamp > threshold and next(measurements) then  --checking whether table is not empty
				resume(child, measurements)
				threshold = os.time() + LAN_INTERVAL
			end

			sensor_id, timestamp, power, counter, msec = coroutine.yield()
		end
	end)
end

function publish(child)
	return coroutine.create(function(measurements)
		nixio.fs.mkdirr(LAN_PUBLISH_PATH)

		for file in nixio.fs.dir(LAN_PUBLISH_PATH) do
			nixio.fs.unlink(file)
		end

		while true do
			measurements:polish(os.time(), LAN_POLISH_CUTOFF)
			local measurements_json = measurements:json_encode(LAN_POLISH_CUTOFF)

			for sensor_id, json in pairs(measurements_json) do
				local file = LAN_PUBLISH_PATH .. '/' .. sensor_id
				
				nixio.fs.unlink(file)
				fd = nixio.open(file, O_RDWR_CREAT)
				fd:write(json)
				fd:close()
			end

			resume(child, measurements)
			measurements = coroutine.yield()
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
				resume(child, measurements)
			end

			measurements = coroutine.yield()
		end
	end)
end

local wan_chain =
	wan_buffer(
		filter(
			filter(
				filter(
					send(
						gc(
							debug(nil)
						)
					)
				, WAN_FILTER[3].span, WAN_FILTER[3].offset)
			, WAN_FILTER[2].span, WAN_FILTER[2].offset)
		, WAN_FILTER[1].span, WAN_FILTER[1].offset)
	)

local lan_chain =
	lan_buffer(
		publish(
			debug(nil)
		)
	)

local chain = dispatch(wan_chain, lan_chain)

resume(chain)
