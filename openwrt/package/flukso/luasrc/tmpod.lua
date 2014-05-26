#!/usr/bin/env lua

--[[
    
    tmpod.lua - Flukso timeseries logging daemon

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

local dbg = require "dbg"
local nixio = require "nixio"
local luci = require "luci"
luci.json = require "luci.json"
luci.util = require "luci.util"
local uci = require "luci.model.uci".cursor()
local uloop = require "uloop"
uloop.init()
local mosq = require "mosquitto"

local DEBUG = {
	block = false
}

local DAEMON = os.getenv("DAEMON") or "tmpod"
local ULOOP_TIMEOUT_MS = 1e3
local SLEEP_S, SLEEP_NS = 1, 0
local TIMESTAMP_MIN = 1234567890
local TMPO_BLOCK8_SPAN = 2^8 --256 secs
local TMPO_BLOCK8_GRACE = 5 --secs TODO randomize!

-- mosquitto client params
local MOSQ_ID = DAEMON
local MOSQ_CLN_SESSION = true
local MOSQ_HOST = "localhost"
local MOSQ_PORT = 1883
local MOSQ_KEEPALIVE = 300
local MOSQ_TIMEOUT = 0 -- return instantly from select call
local MOSQ_MAX_PKTS = 1 -- packets
local MOSQ_QOS0 = 0
local MOSQ_QOS1 = 1
local MOSQ_RETAIN = true

local MOSQ_ERROR = "MQTT error: %s"
local MOSQ_TOPIC_SENSOR_SUB = "/sensor/#"
local MOSQ_TOPIC_SENSOR_PUB = "/sensor/%s/tmpo/8/%s"

-- connect to the MQTT broker
mosq.init()
local mqtt = mosq.new(MOSQ_ID, MOSQ_CLN_SESSION)
while not mqtt:connect(MOSQ_HOST, MOSQ_PORT, MOSQ_KEEPALIVE) do
	nixio.nanosleep(SLEEP_S, SLEEP_NS)
end
mqtt:subscribe(MOSQ_TOPIC_SENSOR_SUB, MOSQ_QOS0)

local tmpo = {
	close8 = nil, --block8 closing time
	block8 = { },

	push = function(self, sid, time, value, unit)
		if not self.block8[sid] then
			self.block8[sid] = { }
		end
		local b8s = self.block8[sid]
		local b8id = math.floor(time / TMPO_BLOCK8_SPAN) * TMPO_BLOCK8_SPAN

		if not b8s[b8id] then
			b8s[b8id] = { t = { time, time }, v = { value, value } }
		else
			local tseries = b8s[b8id].t
			local vseries = b8s[b8id].v
			local tail = #b8s[b8id].t
			tseries[tail] = time - tseries[tail]
			vseries[tail] = value - vseries[tail]
			tseries[tail + 1] = time
			vseries[tail + 1] = value
		end

		if DEBUG.block then dbg.vardump(self.block8) end
	end,

	publish = function(self)
		local time = os.time()
		if time < TIMESTAMP_MIN then return end
		if not self.close8 then
			self.close8 = math.ceil(time / TMPO_BLOCK8_SPAN) * TMPO_BLOCK8_SPAN
		end
		if time < self.close8 + TMPO_BLOCK8_GRACE then return end

		for sid, b8s in pairs(self.block8) do
			for b8id, b8data in pairs(b8s) do
				if b8id < self.close8 then
					local topic = MOSQ_TOPIC_SENSOR_PUB:format(sid, b8id)
					local payload = luci.json.encode(b8data)
					mqtt:publish(topic, payload, MOSQ_QOS1, not MOSQ_RETAIN)
					b8s[b8id] = nil
				end
			end
		end

		self.close8 = math.ceil(time / TMPO_BLOCK8_SPAN) * TMPO_BLOCK8_SPAN
	end
}

mqtt:set_callback(mosq.ON_MESSAGE, function(mid, topic, jpayload, qos, retain)
	if retain then return end
	local sid, stype = topic:match("^/sensor/(%x+)/(%l+)$")
	if not (sid and stype == "counter") then return end --TODO use data_type entry as filter
	local payload = luci.json.decode(jpayload)
	local time, value, unit = payload[1], payload[2], payload[3]
	tmpo:push(sid, time, value, unit)
end)

local ufdr = uloop.fd(mqtt:socket(), uloop.READ, function(events)
		mqtt:read(MOSQ_MAX_PKTS)
    end)

local ufdw = uloop.fd(mqtt:socket(), uloop.WRITE, function(events)
		mqtt:write(MOSQ_MAX_PKTS)
    end)

local ut
ut = uloop.timer(function()
		ut:set(ULOOP_TIMEOUT_MS)
		-- mosquitto connection maintenance
		local success, errno, err = mqtt:misc()
		if not success then error(MOSQ_ERROR:format(err)) end

		-- publish tmpo blocks
		tmpo:publish()
	end, ULOOP_TIMEOUT_MS)

uloop:run()
