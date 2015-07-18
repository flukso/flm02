#!/usr/bin/env lua

--[[

    queryd.lua - Flukso timeseries query daemon
    
    Copyright (C) 2014 Bart Van Der Meerssche <bart@flukso.net>
                  2015 Markus Gebhard <markus.gebhard@web.de>
    
    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program. If not, see <http://www.gnu.org/licenses/>.

]]--

local dbg = require "dbg"
local nixio = require "nixio"
nixio.fs = require "nixio.fs"
local luci = require "luci"
luci.json = require "luci.json"
luci.util = require "luci.util"
local uci = require "luci.model.uci".cursor()
local uloop = require "uloop"
uloop.init()
local ubus = require "ubus"
local ub = assert(ubus.connect(), "unable to connect to ubus")
local mosq = require "mosquitto"

local DEBUG = {
	query = false
}

local DAEMON = os.getenv("DAEMON") or "queryd"
local DEVICE = uci:get_first("system", "system", "device")
local ULOOP_TIMEOUT_MS = 1e3
local SLEEP_S, SLEEP_NS = 1, 0
local TIMESTAMP_MIN = 1234567890

-- TMPO params
local TMPO_FORMAT_VERSION = 1
local TMPO_NICE = 10
local TMPO_BASE_PATH = "/usr/share/tmpo/sensor/"
local TMPO_PATH_TPL = TMPO_BASE_PATH .. "%s/%s/%s/%s" -- [sid]/[rid]/[lvl]/[bid]
local TMPO_REGEX_QUERY = "^/query/(%x+)/tmpo$"
local TMPO_TOPIC_QUERY_PUB = "/sensor/%s/query/%s/%s" -- provide queried data as payload
local TMPO_TOPIC_QUERY_SUB = "/query/+/tmpo" -- get sensor to query with payload interval
local TMPO_FMT_QUERY = "time:%d sid:%s rid:%d lvl:%2d bid:%d"
local TMPO_LVLS_REVERSE = { 20, 16, 12, 8 } -- query runs from the past to now...

-- mosquitto client params
local MOSQ_ID = DAEMON
local MOSQ_CLN_SESSION = true
local MOSQ_HOST = "localhost"
local MOSQ_PORT = 1883
local MOSQ_KEEPALIVE = 900
local MOSQ_TIMEOUT = 0 -- return instantly from select call
local MOSQ_MAX_PKTS = 1 -- packets
local MOSQ_QOS0 = 0 -- at most once
local MOSQ_QOS1 = 1 -- at least once
local MOSQ_QOS2 = 2 -- exactly once
local MOSQ_RETAIN = true
local MOSQ_ERROR = "MQTT error: %s"

-- increase process niceness
nixio.nice(TMPO_NICE)

-- connect to the MQTT broker
mosq.init()
local mqtt = mosq.new(MOSQ_ID, MOSQ_CLN_SESSION)
while not mqtt:connect(MOSQ_HOST, MOSQ_PORT, MOSQ_KEEPALIVE) do
	nixio.nanosleep(SLEEP_S, SLEEP_NS)
end
-- subscribe to query topic
mqtt:subscribe(TMPO_TOPIC_QUERY_SUB, MOSQ_QOS0)

mqtt:set_callback(mosq.ON_MESSAGE, function(mid, topic, jpayload, qos, retain)
	local function sdir(path)
		local files = { }
		for file in nixio.fs.dir(path) or function() end do --dummy iterator
			files[#files + 1] = tonumber(file) or file
		end
		table.sort(files)
		return files
	end

	local function dprint(fmt, ...)
		if DEBUG.query then
			print(fmt:format(
				os.time(),
			...))
		end
	end

	local function publish(sid, rid, lvl, bid, from, to)
		dprint(TMPO_FMT_QUERY, sid, rid, lvl, bid)
		local path = TMPO_PATH_TPL:format(sid, rid, lvl, bid)
		local source = assert(io.open(path, "r"))
		local payload = source:read("*all")
		local topic = TMPO_TOPIC_QUERY_PUB:format(sid, from, to)
		if DEBUG.query then
			local str = string.format("publishing topic:%s payload:%s", topic, payload)
			print(str)
		end
		-- query is published exactly once - QoS = 2
		mqtt:publish(topic, payload, MOSQ_QOS2, not MOSQ_RETAIN)
		source:close()
	end

	-- publish the stored files on a query request
	local function query(sid)
		if not sid then return end
		-- payload contains query time interval [fromtimestamp, totimestamp]
		local payload = luci.json.decode(jpayload)
		if payload == nil then return end
                local lastrid = 0
                local lastlvl = 0
		local lastbid = 0
		local published = false
		local from = payload[1]
		local to = payload[2]
		if DEBUG.query then
			local str = string.format("entered sensor:%s from:%s to:%s", sid, from, to)
			print(str)
		end
		for rid in nixio.fs.dir(TMPO_BASE_PATH .. sid) do
			for _, lvl in ipairs(TMPO_LVLS_REVERSE) do
				for _, bid in ipairs(sdir(TMPO_PATH_TPL:format(sid, rid, lvl, ""))) do
					-- detect store with containing or overlapping values
					if ((from <= bid) and (bid <= to)) then
                                                publish(sid, rid, lvl, bid, from, to)
						published = true
					end
					if ((lastbid ~= 0) and (lastbid < from) and (bid > from)) then
						publish(sid, rid, lastlvl, lastbid, from, to)
                                        	published = true
                                        end
                                        -- recognize overlaps in different compression stages
                                        lastrid = rid
                                        lastlvl = lvl
					lastbid = bid
					if DEBUG.query then
						str = string.format("processed file /%s/%s/%s", rid, lvl, bid)
						print(str)
					end
				end
			end
	end
		-- send last stored file in case there were no further readings, e.g. on solar
		if ((published == false) and (lastbid < from)) then
			publish(sid, lastrid, lastlvl, lastbid, from, to)
		end
		return true
	end

	if retain then return end
	local q = query(topic:match(TMPO_REGEX_QUERY))
	return
end)

local ufdr = uloop.fd(mqtt:socket(), uloop.READ, function(events)
	mqtt:read(MOSQ_MAX_PKTS)
end)

local ufdw = uloop.fd(mqtt:socket(), uloop.WRITE, function(events)
	mqtt:write(MOSQ_MAX_PKTS)
end)

local ub_events = {
	["flukso.sighup"] = function(msg)
		-- do someting meaningful
	end
}

ub:listen(ub_events)

local ut
ut = uloop.timer(function()
	-- mosquitto connection maintenance
	local success, errno, err = mqtt:misc()
	if not success then 
		error(MOSQ_ERROR:format(err)) 
	end
	ut:set(ULOOP_TIMEOUT_MS)
     end, ULOOP_TIMEOUT_MS)

uloop:run()
