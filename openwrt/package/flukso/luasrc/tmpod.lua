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
nixio.fs = require "nixio.fs"
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

local TMPO_NICE = 10
local TMPO_BLOCK8_SPAN = 2^8 --256 secs
local TMPO_BLOCK12_SPAN = 2^12 -- 68 mins
local TMPO_BLOCK16_SPAN = 2^16 -- 18 hours
local TMPO_BLOCK20_SPAN = 2^20 -- 12 days
local TMPO_CLOSE8_GRACE = 5 --secs TODO randomize!
local TMPO_BASE_PATH = "/usr/share/tmpo/sensor/"
local TMPO_PATH_TPL = TMPO_BASE_PATH .. "%s/%s/%s" -- xyz/8/1401108736
local TMPO_REGEX_BLOCK = '^{"h":(.+),"t":%[0(.+)%],"v":%[0(.+)%]}$'
local TMPO_FMT_CONCAT = '{"h":%s,"t":%s,"v":%s}'

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

-- increase process niceness
nixio.nice(TMPO_NICE)

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

	push8 = function(self, sid, time, value, unit)
		if not self.block8[sid] then
			self.block8[sid] = { }
		end
		local b8s = self.block8[sid]
		local b8id = math.floor(time / TMPO_BLOCK8_SPAN) * TMPO_BLOCK8_SPAN

		if not b8s[b8id] then
			b8s[b8id] = {
				h = { head = { time, value }, tail = { time, value } },
				t = { 0 },
				v = { 0 }
			}
		else
			local b8data = b8s[b8id]
			if time > b8data.h.tail[1] then -- ts must increase monotonously
				local n = #b8data.t
				b8data.t[n + 1] = time - b8data.h.tail[1]
				b8data.v[n + 1] = value - b8data.h.tail[2]
				b8data.h.tail[1] = time
				b8data.h.tail[2] = value
			end
		end

		if DEBUG.block then dbg.vardump(self.block8) end
	end,

	flush8 = function(self)
		local time = os.time()
		if time < TIMESTAMP_MIN then return false end
		if not self.close8 then
			self.close8 = math.ceil(time / TMPO_BLOCK8_SPAN) * TMPO_BLOCK8_SPAN
		end
		if time < self.close8 + TMPO_CLOSE8_GRACE then return false end

		for sid, b8s in pairs(self.block8) do
			nixio.fs.mkdirr(TMPO_PATH_TPL:format(sid, 8, ""))

			for b8id, b8data in pairs(b8s) do
				if b8id < self.close8 then
					local topic = MOSQ_TOPIC_SENSOR_PUB:format(sid, b8id)
					local payload = luci.json.encode(b8data)
					mqtt:publish(topic, payload, MOSQ_QOS1, not MOSQ_RETAIN)
					b8s[b8id] = nil

					nixio.fs.writefile(TMPO_PATH_TPL:format(sid, 8, b8id),
						luci.json.encode(b8data))
				end
			end
		end

		self.close8 = math.ceil(time / TMPO_BLOCK8_SPAN) * TMPO_BLOCK8_SPAN
		return true
	end,

	compact = function(self)
		local function sdir(path) --return a sorted array of (int) dir entries
			local files = { }
			for file in nixio.fs.dir(path) do
				files[#files + 1] = tonumber(file) or file
			end
			table.sort(files)
			return files
		end

		local function must_compact(bid, time, lvl)
			local span = 2 ^ lvl
			return math.floor(bid / span) < math.floor(time / span)
		end

		local function in_same_compaction(bid1, bid2, lvl)
			local span = 2 ^ lvl
			return math.floor(bid1 / span) == math.floor(bid2 / span)
		end

		local function pop(sid, lvl, bids, bids_rm, first)
			local bid = table.remove(bids, 1)
			bids_rm[#bids_rm + 1] = bid
			local span = 2 ^ (lvl + 4)
			local bid_compact = math.floor(bid / span) * span
			local jblock = nixio.fs.readfile(TMPO_PATH_TPL:format(sid, lvl, bid))
			local eblock = { } --editable block
			eblock.h, eblock.t, eblock.v = jblock:match(TMPO_REGEX_BLOCK)
			eblock.h = luci.json.decode(eblock.h)

			if first then
				eblock.t = { "[0", eblock.t, "]" }
				eblock.v = { "[0", eblock.v, "]" }
			end
			return bid, eblock, bid_compact
		end

		local function join(eblock1, eblock2)
			local dtime = eblock2.h.head[1] - eblock1.h.tail[1]
			local dvalue = eblock2.h.head[2] - eblock1.h.tail[2]
			table.insert(eblock1.t, #eblock1.t, "," .. dtime)
			table.insert(eblock1.v, #eblock1.v, "," .. dvalue)
			table.insert(eblock1.t, #eblock1.t, eblock2.t)
			table.insert(eblock1.v, #eblock1.v, eblock2.v)
			eblock1.h.tail = eblock2.h.tail
			return eblock1
		end

		local function concat(eblock)
			local jblock = {
				h = luci.json.encode(eblock.h),
				t = table.concat(eblock.t),
				v = table.concat(eblock.v)
			}			
			return TMPO_FMT_CONCAT:format(jblock.h, jblock.t, jblock.v)
		end

		local function rm(sid, lvl, bids)
			for _, bid in ipairs(bids) do
				nixio.fs.unlink(TMPO_PATH_TPL:format(sid, lvl, bid))
			end
		end

		local function scan(sid, lvl, time)
			nixio.fs.mkdirr(TMPO_PATH_TPL:format(sid, lvl + 4, ""))
			local bids = sdir(TMPO_PATH_TPL:format(sid, lvl, ""))
			while #bids > 0 and must_compact(bids[1], time, lvl + 4) do
				local bids_rm = { }
				local bid, eblock, bid_compact = pop(sid, lvl, bids, bids_rm, true)
				while #bids > 0 and in_same_compaction(bid_compact, bids[1], lvl + 4) do
					local bidn, eblockn =  pop(sid, lvl, bids, bids_rm, false)
					eblock = join(eblock, eblockn)
				end

				local jblock = concat(eblock)
				nixio.fs.writefile(TMPO_PATH_TPL:format(sid, lvl + 4, bid_compact), jblock)
				rm(sid, lvl, bids_rm)
			end
		end

		local time = os.time()
		if time < TIMESTAMP_MIN then return false end

		for sid in nixio.fs.dir(TMPO_BASE_PATH) do
			for _, lvl in ipairs(sdir(TMPO_BASE_PATH .. sid)) do --possibly 8/12/16/20
				if lvl < 20 then scan(sid, lvl, time) end
			end
		end
	end
}

mqtt:set_callback(mosq.ON_MESSAGE, function(mid, topic, jpayload, qos, retain)
	if retain then return end
	local sid, stype = topic:match("^/sensor/(%x+)/(%l+)$")
	if not (sid and stype == "counter") then return end --TODO use data_type entry as filter
	local payload = luci.json.decode(jpayload)
	local time, value, unit = payload[1], payload[2], payload[3]
	tmpo:push8(sid, time, value, unit)
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

		-- tmpo block servicing
		if tmpo:flush8() then tmpo:compact() end
	end, ULOOP_TIMEOUT_MS)

uloop:run()
