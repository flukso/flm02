#!/usr/bin/env lua

--[[
    
    kubed.lua - The FluksoKube daemon

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
nixio.util = require "nixio.util"
local luci = require "luci"
luci.json = require "luci.json"
local uci = require "luci.model.uci".cursor()
local ubus = require "ubus"
local uloop = require "uloop"
local vstruct = require "vstruct"
vstruct.cache = true
local rfsm = require "rfsm"
rfsm.pp = require "rfsm.pp"
rfsm.timeevent = require "rfsm.timeevent"

local state, trans, conn = rfsm.state, rfsm.trans, rfsm.conn

local KUBE, REGISTRY, SENSOR
local O_RDONLY = nixio.open_flags("rdonly")
local ULOOP_TIMEOUT = 1000 --ms

local FMT_HEADER = "< grp:u1 [1| ctl:b1 dst:b1 ack:b1 nid:u5] len:u1"
local FMT_PAIR_REQUEST = "< @3 hw_type:u2 grp:u1 nid:u1 check:u2 hw_id:s16"
local FMT_PAIR_REPLY = "" --TODO

local DEBUG = {
	config = false,
	decode = false,
	encode = false
}

local function rfsm_gettime()
	local secs, usecs = nixio.gettimeofday()
	return secs, usecs * 1e3
end

rfsm.timeevent.set_gettime_hook(rfsm_gettime)

local function load_config()
	KUBE = uci:get_all("kube")
	SENSOR = uci:get_all("flukso")

	local fd = nixio.open(KUBE.main.registry_cache, O_RDONLY)
	local registry = fd:readall()
	REGISTRY = luci.json.decode(registry)
	fd:close()

	--TODO raise error when KUBE, SENSOR or REGISTRY are nil

	if DEBUG.config then
		dbg.vardump(KUBE)
		dbg.vardump(SENSOR)
		dbg.vardump(REGISTRY)
	end
end

local function decode(script, pkt)
	if type(script) == "string" then
		vstruct.unpack(script, pkt.bin, pkt.pld)
	elseif type(script) == "table" then
		if script.type_bits == 0 then
		else
		end
	end

	if DEBUG.decode then dbg.vardump(pkt.pld) end
end

local function encode(format, data)
	return vstruct.pack(format, data)
end

local function send(pkt)
	local pkt_hex = nixio.bin.hexlify(pkt)
	ub:send("flukso.kubed.packet.tx", { pkt = pkt_hex } )
end

local function reply(hdr, format, pld)
	local hdr_reply = hdr -- TODO change relevant stuff in the header
	send(encode(FMT_HEADER, hdr) .. encode(format, pld))
end

local function is_not_kube_provisioned(hw_id_hex)
	for kid, values in pairs(KUBE) do
		if values.hw_id and values.hw_id == hw_id_hex then
			return false
		end
	end

	return true
end

local function provision_kube(hw_id, hw_type)
	local function is_hw_type_registered(hw_type)
		return REGISTRY[tostring(hw_type)]
	end

	local function latest_firmware(hw_type)
		local latest = 0

		for k, v in pairs(REGISTRY[tostring(hw_type)]) do
			if tonumber(k) and tonumber(k) > latest then
				latest = tonumber(k)
			end
		end

		return latest
	end
                                                            
	local function sensor_count(hw_type) 
		local total = 0
		local firmware = latest_firmware(hw_type)
		for k, v in pairs(REGISTRY[tostring(hw_type)][tostring(firmware)].decode.sensors) do
			total = total + 1
		end

		return total
	end

	-- works for both KUBE and SENSOR data
	local function get_free(list)
		local range = list.main and
			(list.main.max_provisioned_sensors or list.main.max_nodes)
		if not range then return nil end

		local first_free, total_free = nil, range

		for i = 1, range do
			if list[tostring(i)] and list[tostring(i)].enable then
				total_free = total_free - 1
			elseif not first_free then
				first_free = i
			end
		end

		return first_free, total_free
	end

	local kid = get_free(KUBE)
	local sensor, free_sensors = get_free(SENSOR)

	if not is_hw_type_registered(hw_type) then
		--TODO raise error
	elseif not kid then
		--TODO raise error
	elseif sensor_count(hw_type) > free_sensors then
		--TODO raise error
	else
		local values = {
			hw_id = nixio.bin.hexlify(hw_id),
			hw_type = hw_type,
			sw_version = 0,
			enable = 1
		}

		uci:section("kube", "node", tostring(kid), values)
		uci:save("kube")
		uci:commit("kube")
		KUBE = uci:get_all("kube")

		for sensor_type in pairs(REGISTRY[tostring(hw_type)]
			[tostring(latest_firmware(hw_type))].decode.sensors) do
			local values = {
				class = "kube",
				["type"] = sensor_type,
				kid = kid,
				enable = 1
			}

			uci:tset("flukso", tostring(get_free(SENSOR)), values)
			uci:save("flukso")
			SENSOR = uci:get_all("flukso")
		end
		uci:commit("flukso")
	end
end

local function deprovision_kube(kid)
	uci:foreach("flukso", "sensor", function(sensor)
		if sensor.kid == tostring(kid) then
			local sidx = sensor[".name"]
			local entries = { "class", "type", "function", "kid", "enable" }
			for i, entry in ipairs(entries) do
				uci:delete("flukso", sidx, entry)
			end
		end
	end)
	uci:save("flukso")
	uci:commit("flukso")

	uci:delete("kube", tostring(kid))
	uci:save("kube")
	uci:commit("kube")
end

local function is_kid_allocated(kid)
	return type(KUBE[tostring(kid)]) == "table"
end

-- rFSM event parameters
local e_arg

local root = state {
	dbg = false,

	collecting = state {
		entry = function()
				load_config()
				--TODO listen on collecting grp
			end,

		-- rFSM does not support internal transitions
		-- resorting to a nested state without entry/exit actions as a workaround
		decoding = state { },

		trans { src = "initial", tgt = "decoding" },
		trans { src = "decoding", tgt = "decoding", events = { "e_packet" },
				effect = decode(collect_script, e_arg) }, --TODO map to JSON packet description
	},

	pairing = state {
		entry = function()
				-- TODO listen on pairing grp 212
			end,

		decoding = state { },
		pair_request = state {
			entry = function()
				decode(FMT_PAIR_REQUEST, e_arg)
			end
		},
		pair_reply = state {
			entry = function()
				local pld = { } --TODO fill in the blanks
				reply(e_arg.hdr, FMT_PAIR_REPLY, pld)
			end
		},
		provision = state {
			entry = function()
				provision_kube(e_arg.pld.hw_id, e_arg.pld.hw_type)
			end
		},

		trans { src = "initial", tgt = "decoding" },
		trans { src = "decoding", tgt = "pair_request", events = { "e_packet_oam" },
			guard = function()
				return e_arg and e_arg.head.len == 22
			end
		},
		trans { src = "pair_request", tgt = "provision", events = { "e_done" }, pn = 1,
			guard = function()
				return is_not_kube_provisioned(nixio.bin.hexlify(e_arg.pld.hw_id))
			end
		},
		trans { src = "pair_request", tgt = "pair_reply", events = { "e_done" }, pn = 0 },
		trans { src = "provision", tgt = "pair_reply", events = { "e_done" } },
	},

	deprovision = state {
		entry = function()
			deprovision_kube(e_arg)
		end
	},

	trans { src = "initial", tgt = "collecting" },
	trans { src = "collecting", tgt = "pairing", events = { "e_pair" } },
	trans { src = "pairing", tgt = "collecting", events = { "e_after(30)" } },
	trans { src = ".pairing.pair_reply", tgt = "collecting", events = { "e_done" } },
	trans { src = "collecting", tgt = "deprovision", events = { "e_deprovision" },
		guard = function()
			return type(e_arg) == "number" and is_kid_allocated(e_arg)
		end
	},
	trans { src = "deprovision", tgt = "collecting", events = { "e_done" } }
}

local fsm = rfsm.init(root)
rfsm.run(fsm)

uloop.init()
local ub = assert(ubus.connect(), "unable to connect to ubus")

local ub_methods = {
	["flukso.kubed"] = {
		debug = {
			function(req, msg)
				if type(msg.fsm) == "boolean" then
					if msg.fsm then
						fsm.dbg = rfsm.pp.gen_dbgcolor("kubed", {
							STATE_ENTER = true,
							STATE_EXIT = true,
							EFFECT = false,
							DOO = false,
							EXEC_PATH = false,
							ERROR = true,
							HIBERNATING = false,
							RAISED = true,
							TIMEEVENT = false
						}, false)
					else
						fsm.dbg = function() return end
					end
				end

				if type(msg.config) == "boolean" then
					DEBUG.config = msg.config
				end

				if type(msg.decode) == "boolean" then
					DEBUG.decode = msg.decode
				end

				if type(msg.encode) == "boolean" then
					DEBUG.encode = msg.encode
				end

				ub:reply(req, { success = true, msg = "kubed debugging flags updated" })
			end, { fsm = ubus.BOOLEAN, decode = ubus.BOOLEAN, encode = ubus.BOOLEAN }
		},

		deprovision = {
			function(req, msg)
				e_arg = msg.kid
				rfsm.send_events(fsm, "e_deprovision")
				rfsm.run(fsm)
				local reply = string.format("kube with kid=%d has been deprovisioned", e_arg)
				ub:reply(req, { success = true, msg = reply })
			end, { kid = ubus.INT32 }
		}
	}
}

ub:add(ub_methods)

local ub_events = {
	["flukso.kubed.event"] = function(msg)
		if type(msg.event) == "string" then
			rfsm.send_events(fsm, msg.event)
			rfsm.run(fsm)
		end
	end,

	["flukso.kubed.packet.rx"] = function(msg)
		if type(msg.hex) ~= "string" then return end
		e_arg = { bin = nixio.bin.unhexlify(msg.hex), head = { }, pld = { } }
		vstruct.unpack(FMT_HEADER, e_arg.bin, e_arg.head)

		if DEBUG.decode then dbg.vardump(e_arg) end

		local event
		if e_arg.head.ctl and e_arg.head.ack then
			event = "e_packet_oam"
		else
			event = "e_packet"
		end

		rfsm.send_events(fsm, event)
		rfsm.run(fsm)
	end
}

ub:listen(ub_events)

local ut
ut = uloop.timer(function()
		ut:set(ULOOP_TIMEOUT)
		rfsm.run(fsm)
	end, ULOOP_TIMEOUT)

uloop:run()
