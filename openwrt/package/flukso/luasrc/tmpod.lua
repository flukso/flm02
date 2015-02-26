#!/usr/bin/env lua

--[[
    
    tmpod.lua - Flukso timeseries logging daemon

    Copyright (C) 2015 Bart Van Der Meerssche <bart@flukso.net>

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
local gzio = require "gzio"
local nixio = require "nixio"
nixio.fs = require "nixio.fs"
local luci = require "luci"
luci.json = require "luci.json"
luci.util = require "luci.util"
luci.sys = require "luci.sys"
local uci = require "luci.model.uci".cursor()
local uloop = require "uloop"
uloop.init()
local ubus = require "ubus"
local ub = assert(ubus.connect(), "unable to connect to ubus")
local mosq = require "mosquitto"

local DEBUG = {
	config = false,
	block8 = false,
	compact = false,
	sync = false,
	query = false
}

local DAEMON = os.getenv("DAEMON") or "tmpod"
local DEVICE = uci:get_first("system", "system", "device")
local ULOOP_TIMEOUT_MS = 1e3
local SLEEP_S, SLEEP_NS = 1, 0
local TIMESTAMP_MIN = 1234567890

local TMPO_FORMAT_VERSION = 1
local TMPO_NICE = 10
local TMPO_BLOCK8_SPAN = 2^8 -- 256 secs
local TMPO_BLOCK12_SPAN = 2^12 -- 68 mins
local TMPO_BLOCK16_SPAN = 2^16 -- 18 hours
local TMPO_BLOCK20_SPAN = 2^20 -- 12 days
local TMPO_LVLS_REVERSE = { 20, 16, 12, 8 }
math.randomseed(os.time())
local TMPO_CLOSE8_GRACE = 2^2 + math.floor(math.random() * 2^4) -- 4-20 secs
local TMPO_BASE_PATH = "/usr/share/tmpo/sensor/"
local TMPO_PATH_TPL = TMPO_BASE_PATH .. "%s/%s/%s/%s" -- [sid]/[rid]/[lvl]/[bid]
local TMPO_REGEX_BLOCK = '^{"h":(.+),"t":%[0(.*)%],"v":%[0(.*)%]}$'
local TMPO_FMT_CONCAT = '{"h":%s,"t":%s,"v":%s}'
local TMPO_DBG_COMPACT_INFO = "time:%d flash[4kB]:%d ram[kB]:%.0f sid:%s rid:%d lvl:%2d cid:%d"
local TMPO_DBG_SYNC_INFO = "time:%d flash[4kB]:%d ram[kB]:%.0f sid:%s rid:%d lvl:%2d bid:%d [s]"
local TMPO_BLOCK_SIZE = 4096
local TMPO_REGEX_H = '^{"h":(.+),"t":%[0(.*)$'
local TMPO_REGEX_T = '^(.-)%](.*)$'
local TMPO_REGEX_V1 = '^,"v":%[0(.*)$'
local TMPO_REGEX_V2 = '^(.-)%].*$'
local TMPO_REGEX_SYNC = "^/d/device/(%x+)/tmpo/sync$"
local TMPO_REGEX_SENSOR = "^/sensor/(%x+)/(%l+)$"
local TMPO_TOPIC_SYNC_SUB = "/d/device/%s/tmpo/sync"
local TMPO_TOPIC_SYNC_PUB = "/device/%s/tmpo/sync"
local TMPO_TOPIC_SENSOR_SUB = "/sensor/+/+"
local TMPO_TOPIC_SENSOR_PUB = "/sensor/%s/tmpo/%d/%d/%d/gz"
-- /sensor/[sid]/tmpo/[rid]/[lvl]/[bid]/gz

local TMPO_REGEX_QUERY = "^/query/(%x+)/tmpo$"
local TMPO_TOPIC_QUERY_PUB = "/sensor/%s/query/%s/%s" -- provide queried data as payload
local TMPO_TOPIC_QUERY_SUB = "/query/+/tmpo" -- get sensor to query with payload interval
local TMPO_FMT_QUERY = "time:%d sid:%s rid:%d lvl:%2d bid:%d"

local TMPO_TOPIC_MQTT_CHECK = string.format("/daemon/%s/check", DAEMON)
local TMPO_GC20_THRESHOLD = 100 -- 100 free 4kB blocks out of +-1000 in jffs2 = 90% full
local TMPO_GZCHECK_EXEC_FMT = "gzip -trS '' %s 2>&1"
local TMPO_GZCHECK_FILE_REGEX = "^gzip:%s*([%w%.%-_/]+):.*$"

-- mosquitto client params
local MOSQ_ID = DAEMON
local MOSQ_CLN_SESSION = true
local MOSQ_HOST = "localhost"
local MOSQ_PORT = 1883
local MOSQ_KEEPALIVE = 900
local MOSQ_TIMEOUT = 0 -- return instantly from select call
local MOSQ_MAX_PKTS = 1 -- packets
local MOSQ_QOS0 = 0
local MOSQ_QOS1 = 1
local MOSQ_QOS2 = 2
local MOSQ_RETAIN = true
local MOSQ_ERROR = "MQTT error: %s"

-- increase process niceness
nixio.nice(TMPO_NICE)

local function merror(success, errno, err)
	--TODO explicitely cancel the uloop on error
	if not success then error(MOSQ_ERROR:format(err)) end
end

-- connect to the MQTT broker
mosq.init()
local mqtt = mosq.new(MOSQ_ID, MOSQ_CLN_SESSION)
if not mqtt:connect(MOSQ_HOST, MOSQ_PORT, MOSQ_KEEPALIVE) then
	repeat
		nixio.nanosleep(SLEEP_S, SLEEP_NS)
	until mqtt:reconnect()
end
merror(mqtt:subscribe(TMPO_TOPIC_SYNC_SUB:format(DEVICE), MOSQ_QOS0))
merror(mqtt:subscribe(TMPO_TOPIC_SENSOR_SUB, MOSQ_QOS0))
-- subscribe to query topic
merror(mqtt:subscribe(TMPO_TOPIC_QUERY_SUB, MOSQ_QOS0))

local config = {
	sensor = nil,

	load = function(self)
		local function clean(itbl)
			local otbl = { }
			for sidx, params in pairs(itbl) do
				if tonumber(sidx) -- only interested in sensor entries
				and params.enable
				and params.enable == "1"
				then
					otbl[params.id] = params
					params[".index"] = nil
					params[".name"] = nil
					params[".type"] = nil
					params[".anonymous"] = nil
					params.enable = nil
					params.counter = nil
					if not params.data_type then params.data_type = "counter" end
					if not params.rid then params.rid = 0 end

					for option, value in pairs(params) do
						params[option] = tonumber(value) or value
						if type(value) == "table" then -- dealing with a list
							for i in pairs(value) do
								value[i] = tonumber(value[i]) or value[i]
							end 
						else
							params[option] = tonumber(value) or value
						end
					end
				end
			end
			return otbl
		end

		self.sensor = clean(uci:get_all("flukso"))
		if DEBUG.config then dbg.vardump(self.sensor) end
	end
}

local tmpo = {
	close8 = nil, --block8 closing time
	block8 = { },
	cocompact = nil,
	synclist = nil,

	init = function()
		-- gzip file integrity check
		local function gzcheck()
			local cmd = TMPO_GZCHECK_EXEC_FMT:format(TMPO_BASE_PATH)
			for line in luci.util.execi(cmd) do
				local file = line:match(TMPO_GZCHECK_FILE_REGEX)
				if file then nixio.fs.unlink(file) end
			end
		end

		-- check for blocks that have already been compacted
		local function compactcheck()
			--return a sorted array of (int) dir entries
			local function sdir(path)
				local files = { }
				for file in nixio.fs.dir(path) do
					files[#files + 1] = tonumber(file) or file
				end
				table.sort(files)
				return files
			end

			local function compacted(sid, rid, lvl, bid)
				local function compaction_id(bid, lvl)
					local span = 2 ^ (lvl + 4)
					return math.floor(bid / span) * span
				end

				local cbid = compaction_id(bid, lvl)
				local file = TMPO_PATH_TPL:format(sid, rid, lvl + 4, cbid)
				return (nixio.fs.stat(file) and true) or false
			end

			for sid in nixio.fs.dir(TMPO_BASE_PATH) do
				for _, rid in ipairs(sdir(TMPO_BASE_PATH .. sid)) do
					for _, lvl in ipairs(sdir(TMPO_PATH_TPL:format(sid, rid, "", ""))) do
						if lvl < 20 then
							for _, bid in ipairs(sdir(TMPO_PATH_TPL:format(sid, rid, lvl, ""))) do
								if compacted(sid, rid, lvl, bid) then
									local file = TMPO_PATH_TPL:format(sid, rid, lvl, bid)
									nixio.fs.unlink(file)
								end
							end
						end
					end
				end
			end
		end

		gzcheck()
		compactcheck()
		merror(mqtt:publish(TMPO_TOPIC_SYNC_PUB:format(DEVICE), "", MOSQ_QOS0, not MOSQ_RETAIN))
	end,

	push8 = function(self, sid, time, value, unit)
		if not self.block8[sid] then
			self.block8[sid] = { }
		end
		local b8s = self.block8[sid]
		local b8id = math.floor(time / TMPO_BLOCK8_SPAN) * TMPO_BLOCK8_SPAN
		local params = config.sensor[sid]
		local rid = (params and params.rid) or 0 

		if not b8s[rid] then
			b8s[rid] = { }
		end

		if not b8s[rid][b8id] then
			b8s[rid][b8id] = {
				h = {
					head = { time, value },
					tail = { time, value },
					vsn = TMPO_FORMAT_VERSION,
					cfg = params
				},
				t = { 0 },
				v = { 0 }
			}
		else
			local b8data = b8s[rid][b8id]
			if time > b8data.h.tail[1] then -- ts must increase monotonously
				local n = #b8data.t
				b8data.t[n + 1] = time - b8data.h.tail[1]
				local delta = value - b8data.h.tail[2]
				if math.floor(delta) ~= delta then
					delta = delta + 0.0005
					delta = delta - delta%0.001 -- round to 3 decimals
				end
				b8data.v[n + 1] = delta
				b8data.h.tail[1] = time
				b8data.h.tail[2] = value
			end
		end

		if DEBUG.block8 then dbg.vardump(self.block8) end
	end,

	flush8 = function(self, force)
		local time = os.time()
		-- make sure time > close8 when force == true
		if force then time = time + TMPO_BLOCK8_SPAN + TMPO_CLOSE8_GRACE end

		if time < TIMESTAMP_MIN then return false end
		if not self.close8 then
			self.close8 = math.ceil(time / TMPO_BLOCK8_SPAN) * TMPO_BLOCK8_SPAN
		end
		if time < self.close8 + TMPO_CLOSE8_GRACE then return false end

		for sid, b8s in pairs(self.block8) do
			for rid, b8sr in pairs(b8s) do
				nixio.fs.mkdirr(TMPO_PATH_TPL:format(sid, rid, 8, ""))

				for b8id, b8data in pairs(b8sr) do
					if b8id < self.close8 then
						local path = TMPO_PATH_TPL:format(sid, rid, 8, b8id)
						local sink = assert(gzio.open(path, "w9f"))
						sink:write(luci.json.encode(b8data))
						sink:close()
						b8sr[b8id] = nil

						local source = assert(io.open(path, "r"))
						local payload = source:read("*all")
						local topic = TMPO_TOPIC_SENSOR_PUB:format(sid, rid, 8, b8id)
						merror(mqtt:publish(topic, payload, MOSQ_QOS0, not MOSQ_RETAIN))
						source:close()
					end
				end
			end
		end

		self.close8 = math.ceil(time / TMPO_BLOCK8_SPAN) * TMPO_BLOCK8_SPAN
		return true
	end,

	compact = function(self)
		local function sdir(path) --return a sorted array of (int) dir entries
			local files = { }
			for file in nixio.fs.dir(path) or function() end do --dummy iterator
				files[#files + 1] = tonumber(file) or file
			end
			table.sort(files)
			return files
		end

		local function sibling_bids(sid, rid, lvl, time)
			local bids = sdir(TMPO_PATH_TPL:format(sid, rid, lvl, ""))
			return function() --iterator
				local function must_compact(bid, time, lvl)
					local span = 2 ^ lvl
					return math.floor(bid / span) < math.floor(time / span)
				end

				local function in_same_compaction(bid1, bid2, lvl)
					local span = 2 ^ lvl
					return math.floor(bid1 / span) == math.floor(bid2 / span)
				end

				local cbids = { }
				if #bids > 0 and must_compact(bids[1], time, lvl + 4) then
					cbids[1] = bids[1]
					table.remove(bids, 1)
					while #bids > 0 and in_same_compaction(cbids[1], bids[1], lvl + 4) do
						cbids[#cbids + 1] = bids[1]
						table.remove(bids, 1)
					end
					return cbids
				end
				return nil
			end
		end

		local function run_compaction(sid, rid, lvl, cbids)
			local function dprint(fmt, ...)
				if DEBUG.compact then
					print(fmt:format(
						os.time(),
						nixio.fs.statvfs(TMPO_BASE_PATH).bfree,
						collectgarbage("count"),
						...))
				end
			end

			local function compaction_id(bid, lvl)
				local span = 2 ^ (lvl + 4)
				return math.floor(bid / span) * span
			end

	 		local function rm(sid, rid, lvl, bids)
	    		for _, bid in ipairs(bids) do
		    		nixio.fs.unlink(TMPO_PATH_TPL:format(sid, rid, lvl, bid))
		    	end
	    	end

			local function fsync(path)
				local fs = nixio.open(path, "a")
				fs:sync()
				fs:close()
			end

			local function gzinit(sid, rid, lvl, cbids)
				local function stream(sid, rid, lvl, cbid)
					local path = TMPO_PATH_TPL:format(sid, rid, lvl, cbid)
					local gzfd = assert(gzio.open(path, "r"))
					local state, buffer = "h", nil

					return function() --iterator returning: state, chunk, last
						if not gzfd then
							return nil, "tmpo file streaming completed"
						end

						if not buffer then
							buffer = gzfd:read(TMPO_BLOCK_SIZE)
							if not buffer then
								gzfd:close()
								return state, "", true
							end
						else
							local buffer1 = gzfd:read(TMPO_BLOCK_SIZE)
							if buffer1 then buffer = buffer .. buffer1 end
						end

						local head, tail
						if state == "h" then
							state = "t"
							head, buffer = buffer:match(TMPO_REGEX_H)
							return "h", head, true
						elseif state == "t" then
							head, tail = buffer:match(TMPO_REGEX_T)
							if not head then
								head, buffer = buffer, nil
								return "t", head, false
							else
								state, buffer = "v1", tail
								return "t", head, true
							end
						elseif state == "v1" then
							state, buffer = "v2", buffer:match(TMPO_REGEX_V1)
							return "v", "", false
						elseif state == "v2" then
							head = buffer:match(TMPO_REGEX_V2)
							if not head then
								head, buffer = buffer, nil
								return "v", head, false
							else
								gzfd:close()
								buffer, gzfd = nil, nil
								return "v", head, true
							end
						end
					end
				end

				local sources = { }
				for _, cbid in ipairs(cbids) do
					--if no stream iterator can be created,
					--then it's not added to sources either
					sources[#sources + 1] = stream(sid, rid, lvl, cbid)
				end
				return sources
			end

			local function gzhead(sources, sink)
				local headers = { } 
				for _, stream in ipairs(sources) do
					local state, jchunk, last = stream()
					if state and state == "h" and last then
						headers[#headers + 1] = luci.json.decode(jchunk)
					else
						--TODO report error
					end
				end

				local h = luci.util.clone(headers[#headers])
				h.head = headers[1].head
				sink:write('{"h":', luci.json.encode(h), ',"t":[0')
				return headers
			end

			local function gzdata(key, headers, sources, sink)
				local function delta(j, header1, header2)
					--no delta insertion for the first stream 
					return header1
						and "," .. (header2.head[j] - header1.tail[j])
						or ""
				end

				for i, stream in ipairs(sources) do
					sink:write(delta(key == "t" and 1 or 2,
						headers[i - 1], headers[i]))
					while true do
						local state, chunk, last = stream()
						if state and state == key then
							sink:write(chunk)
						else
							--TODO report error
						end
						if last then break end
					end
				end

				if key == "t" then
					sink:write('],"v":[0')
				elseif key == "v" then
					sink:write(']}')
				end
			end

			nixio.fs.mkdirr(TMPO_PATH_TPL:format(sid, rid, lvl + 4, ""))
			local cid = compaction_id(cbids[1], lvl)	
			local path = TMPO_PATH_TPL:format(sid, rid, lvl + 4, cid)
			if nixio.fs.stat(path) then --never overwrite an existing tmpo block
				rm(sid, rid, lvl, cbids)
				return
			end
			local sources = gzinit(sid, rid, lvl, cbids)
			local sink = assert(gzio.open(path, "w9f"))
			local headers = gzhead(sources, sink)
			gzdata("t", headers, sources, sink)
			gzdata("v", headers, sources, sink)
			sink:close()
			--gzio does not have an fsync method on a file handle
			fsync(path)
			rm(sid, rid, lvl, cbids)
			dprint(TMPO_DBG_COMPACT_INFO, sid, rid, lvl + 4, cid)

			local source = assert(io.open(path, "r"))
			local payload = source:read("*all")
			local topic = TMPO_TOPIC_SENSOR_PUB:format(sid, rid, lvl + 4, cid)
			merror(mqtt:publish(topic, payload, MOSQ_QOS0, not MOSQ_RETAIN))
			source:close()
		end

		local time = os.time()
		if time < TIMESTAMP_MIN then return end

		return coroutine.wrap(function()
			local costart, costop = os.time(), nil

			for sid in nixio.fs.dir(TMPO_BASE_PATH) do
				for _, rid in ipairs(sdir(TMPO_BASE_PATH .. sid)) do
					for _, lvl in ipairs(sdir(TMPO_PATH_TPL:format(sid, rid, "", ""))) do
						--can be 8/12/16/20
						if lvl < 20 then
							for cbids in sibling_bids(sid, rid, lvl, time) do
								run_compaction(sid, rid, lvl, cbids)
								collectgarbage()
								-- cut tmpod some slack to catch up
								-- with queued mqtt sensor readings
								costop = os.time()
								while os.time() < costop + 1 + (costop - costart) / 2 do
									self.close8 =
										math.ceil(os.time() / TMPO_BLOCK8_SPAN + 0.5) *
										TMPO_BLOCK8_SPAN
									coroutine.yield(true)
								end
								costart = os.time()
							end
						end
					end
				end
			end
		end)
	end,

	gc20 = function(self)
		if nixio.fs.statvfs(TMPO_BASE_PATH).bfree > TMPO_GC20_THRESHOLD then
			return false
		end
	
		local block20, oldest = { }, nil
		for sid in nixio.fs.dir(TMPO_BASE_PATH) do
			for rid in nixio.fs.dir(TMPO_BASE_PATH .. sid) do
				for bid in nixio.fs.dir(TMPO_PATH_TPL:format(sid, rid, 20, "")) do
					local bidn = tonumber(bid)
					local path = TMPO_PATH_TPL:format(sid, rid, 20, bid)
					block20[path] = bidn
					if (not oldest) or bidn < oldest then oldest = bidn end
				end
			end
		end

		if oldest then
			for path, bid in pairs(block20) do
				if bid == oldest then
					nixio.fs.unlink(path)
				end
			end
		else
			--TODO flash nearly full with no block20's to erase
		end
		return true
	end,

	sync1 = function(self, synclist)
		self.synclist = synclist
	end,

	sync2 = function(self)
		local function dprint(fmt, ...)
			if DEBUG.sync then
				print(fmt:format(
					os.time(),
					nixio.fs.statvfs(TMPO_BASE_PATH).bfree,
					collectgarbage("count"),
					...))
			end
		end

		local function tail(lvl, bid)
			return bid + 2^lvl - 1
		end

		local function sdir(path)
			local files = { }
			for file in nixio.fs.dir(path) or function() end do --dummy iterator
				files[#files + 1] = tonumber(file) or file
			end
			table.sort(files)
			return files
		end

		local function publish(sid, rid, lvl, bid)
			dprint(TMPO_DBG_SYNC_INFO, sid, rid, lvl, bid)
			local path = TMPO_PATH_TPL:format(sid, rid, lvl, bid)
			local source = assert(io.open(path, "r"))
			local payload = source:read("*all")
			local topic = TMPO_TOPIC_SENSOR_PUB:format(sid, rid, lvl, bid)
			merror(mqtt:publish(topic, payload, MOSQ_QOS0, not MOSQ_RETAIN))
			source:close()
		end

		if not self.synclist then return end
		for _, s in ipairs(self.synclist) do
			local stail = tail(s.lvl, s.bid)
			for _, rid in ipairs(sdir(TMPO_BASE_PATH .. s.sid)) do
				if rid >= s.rid then
					for _, lvl in ipairs(TMPO_LVLS_REVERSE) do
						for _, bid in ipairs(sdir(TMPO_PATH_TPL:format(s.sid, rid, lvl, ""))) do
							local btail = tail(lvl, bid)
							if btail > stail then publish(s.sid, rid, lvl, bid) end
						end
					end
				end
			end
		end
		self.synclist = nil
	end,

	-- mqtt connection servicing
	misc = function(self)
		merror(mqtt:misc())
		merror(mqtt:publish(TMPO_TOPIC_MQTT_CHECK, "", MOSQ_QOS0, not MOSQ_RETAIN))
	end
}

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
                merror(mqtt:publish(topic, payload, MOSQ_QOS2, not MOSQ_RETAIN))
                source:close()
        end

	local function sensor(sid, dtype)
		local sparams = config.sensor[sid]
		if not (sid and sparams and dtype == sparams.data_type) then return end
		local payload = luci.json.decode(jpayload)
		local time, value, unit = payload[1], payload[2], payload[3]
		tmpo:push8(sid, time, value, unit)
		return true
	end

	local function sync(device)
		if not (device and device == DEVICE) then return end
		local payload = luci.json.decode(jpayload)
		tmpo:sync1(payload)
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
                        for _, lvl in ipairs(TMPO_LVLS_REVERSE) do -- storage has to be queried from past to now
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
	if not sensor(topic:match(TMPO_REGEX_SENSOR)) then
        	if not query(topic:match(TMPO_REGEX_QUERY)) then
            		sync(topic:match(TMPO_REGEX_SYNC))
        	end
	end
end)

local ufdr = uloop.fd(mqtt:socket(), uloop.READ, function(events)
	merror(mqtt:read(MOSQ_MAX_PKTS))
end)

local ufdw = uloop.fd(mqtt:socket(), uloop.WRITE, function(events)
	merror(mqtt:write(MOSQ_MAX_PKTS))
end)

local ub_methods = {
	["flukso.tmpo"] = {
		flush = {
			function(req, msg)
				if tmpo:flush8(true) then
					ub:reply(req, { success = true, msg = "tmpo blocks flushed" })
				else
					ub:reply(req, { success = false, msg = "tmpo block flushing failed" })
				end
			end, { }
		}
	}
}

ub:add(ub_methods)

local ub_events = {
	["flukso.sighup"] = function(msg)
		config:load()
	end
}

ub:listen(ub_events)

local ut
ut = uloop.timer(function()
		-- mosquitto connection maintenance
		tmpo:misc()

		-- run sync algo if needed
		tmpo:sync2()

		-- tmpo block servicing
		if tmpo:flush8(false) and not tmpo.cocompact then
			tmpo.cocompact = tmpo:compact() -- returns a coroutine!
		end
		if tmpo.cocompact then
			tmpo:gc20()
			if not tmpo:cocompact() then tmpo.cocompact = nil end
		end

		ut:set(ULOOP_TIMEOUT_MS)
	end, ULOOP_TIMEOUT_MS)

config:load()
tmpo:init()
uloop:run()
