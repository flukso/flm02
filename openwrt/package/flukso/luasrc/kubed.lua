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
local uloop = require "uloop"
uloop.init()
local ubus = require "ubus"
local ub = assert(ubus.connect(), "unable to connect to ubus")
local vstruct = require "vstruct"
vstruct.cache = true
local rfsm = require "rfsm"
rfsm.pp = require "rfsm.pp"
rfsm.timeevent = require "rfsm.timeevent"

local function rfsm_gettime()
	local secs, usecs = nixio.gettimeofday()
	return secs, usecs * 1e3
end

rfsm.timeevent.set_gettime_hook(rfsm_gettime)

local hex, unhex = nixio.bin.hexlify, nixio.bin.unhexlify
local state, trans, conn = rfsm.state, rfsm.trans, rfsm.conn

local KUBE, SENSOR, REGISTRY
local FIRMWARE = {}
local FIRMWARE_BLOCK_SIZE = 64
local O_RDONLY = nixio.open_flags("rdonly")
local ULOOP_TIMEOUT = 1000 --ms

local FMT_HEADER = "< grp:u1 [1| ctl:b1 dst:b1 ack:b1 nid:u5] len:u1"
local FMT_PAIR_REQUEST = "< hw_type:u2 grp:u1 nid:u1 check:u2 hw_id:s16"
local FMT_PAIR_REPLY = "< hw_type:u2 grp:u1 nid:u1 key:s16"
local FMT_UPGRADE_REQUEST = "< hw_type:u2 sw_version:u2 sw_size:u2 sw_crc:u2"
local FMT_UPGRADE_REPLY = FMT_UPGRADE_REQUEST
local FMT_DOWNLOAD_REQUEST = "< sw_version:u2 sw_index:u2"
local FMT_DOWNLOAD_REPLY = "< sw_xor:u2 sw_block:s"

local DEBUG = {
	config = false,
	decode = false,
	encode = false
}

local function load_config()
	KUBE = uci:get_all("kube")
	SENSOR = uci:get_all("flukso")

	local fd = nixio.open(KUBE.main.cache .. "/registry", O_RDONLY)
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

local function send(hdr, format, pld)
	local pld_enc = encode(format, pld)
	hdr.len = #pld_enc
	local hdr_enc = encode(FMT_HEADER, hdr)
	local pkt = hdr_enc .. pld_enc

	if DEBUG.encode then
		dbg.vardump(hdr)
		dbg.vardump(pld)
	end

	return ub:send("flukso.kube.packet.tx", { hex = hex(pkt) })
end

local function reply(hdr, format, pld)
	if hdr.ctl and hdr.ack then --OAM packet
		hdr.dst = not hdr.dst
	end

	return send(hdr, format, pld)
end

local function is_not_kube_provisioned(hw_id_hex)
	for kid, values in pairs(KUBE) do
		if values.hw_id and values.hw_id == hw_id_hex then
			return false
		end
	end

	return true
end

local function latest_kube_firmware(hw_type_s)
	local latest = 0

	for k, v in pairs(REGISTRY[hw_type_s]) do
		if tonumber(k) and tonumber(k) > latest then
			latest = tonumber(k)
		end
	end

	return tostring(latest)
end

local function provision_kube(hw_id_hex, hw_type_s)
	local function is_hw_type_registered(hw_type_s)
		return REGISTRY[hw_type_s]
	end

	local function sensor_count(hw_type_s)
		local total = 0
		local sw_version_s = latest_kube_firmware(hw_type_s)
		for k, v in pairs(REGISTRY[hw_type_s][sw_version_s].decode.sensors) do
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

		return first_free and tostring(first_free), total_free
	end

	local kid_s = get_free(KUBE)
	local sensor_s, free_sensors = get_free(SENSOR)

	if not is_hw_type_registered(hw_type_s) then
		--TODO raise error
	elseif not kid_s then
		--TODO raise error
	elseif sensor_count(hw_type_s) > free_sensors then
		--TODO raise error
	else
		local values = {
			hw_id = hw_id_hex,
			hw_type = hw_type_s,
			sw_version = 0,
			enable = 1
		}

		uci:section("kube", "node", kid_s, values)
		uci:save("kube")
		uci:commit("kube")
		KUBE = uci:get_all("kube")

		for sensor_type_s in pairs(REGISTRY[hw_type_s]
			[latest_kube_firmware(hw_type_s)].decode.sensors) do
			local values = {
				class = "kube",
				["type"] = sensor_type_s,
				kid = kid_s,
				enable = 1
			}

			uci:tset("flukso", get_free(SENSOR), values)
			uci:save("flukso")
			SENSOR = uci:get_all("flukso")
		end
		uci:commit("flukso")

		return tonumber(kid_s)
	end

	return nil
end

local function deprovision_kube(kid_s)
	uci:foreach("flukso", "sensor", function(sensor)
		if sensor.kid == kid_s then
			local sidx = sensor[".name"]
			local entries = { "class", "type", "function", "kid", "enable" }
			for i, entry in ipairs(entries) do
				uci:delete("flukso", sidx, entry)
			end
		end
	end)
	uci:save("flukso")
	uci:commit("flukso")

	uci:delete("kube", kid_s)
	uci:save("kube")
	uci:commit("kube")
end

local function is_kid_allocated(kid_s, hw_type_s)
	if hw_type_s then
		return type(KUBE[kid_s]) == "table" and KUBE[kid_s].hw_type == hw_type_s
	else
		return type(KUBE[kid_s]) == "table"
	end
end

local function get_kid(hw_id_hex)
	for kid, values in pairs(KUBE) do
		if values.hw_id and values.hw_id == hw_id_hex then
			return tonumber(kid)
		end
	end

	return nil
end

local function update_kube_sw_version(kid_s, sw_version_s)
	if KUBE[kid_s].sw_version ~= sw_version_s then
		uci:set("kube", kid_s, "sw_version", sw_version_s)
		uci:save("kube")
		uci:commit("kube")
		KUBE = uci:get_all("kube")
	end
end

local function kube_target_sw(hw_type_s)
	local sw_version_s = latest_kube_firmware(hw_type_s)
	local sw_size = REGISTRY[hw_type_s][sw_version_s].size
	local sw_crc = REGISTRY[hw_type_s][sw_version_s].crc
	return tonumber(sw_version_s), sw_size, sw_crc
end

local function get_firmware_block(hw_type, sw_version, sw_index)
	if not FIRMWARE[hw_type] then FIRMWARE[hw_type] = { } end
	if not FIRMWARE[hw_type][sw_version] then
		local path = string.format("%s/firmware/%d.%d", KUBE.main.cache, hw_type, sw_version)
		local fd = nixio.open(path, O_RDONLY)
		if not fd then return nil end --TODO raise error
		FIRMWARE[hw_type][sw_version] = fd:readall()
		fd:close()
	end

	local size = FIRMWARE[hw_type][sw_version]:len()
	local start = sw_index * FIRMWARE_BLOCK_SIZE + 1
	if start > size then return nil end
	local finish = (sw_index + 1) * FIRMWARE_BLOCK_SIZE
	if finish > size then finish = size end
	return FIRMWARE[hw_type][sw_version]:sub(start, finish)
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
		receiving = state { },
		upgrade = state {
			entry = function()
				decode("@3 " .. FMT_UPGRADE_REQUEST, e_arg)
				local kid_s = tostring(e_arg.hdr.nid)
				local hw_type_s = tostring(e_arg.pld.hw_type)
				if not is_kid_allocated(kid_s, hw_type_s) then return end --TODO raise error
				local sw_version_s = tostring(e_arg.pld.sw_version)
				update_kube_sw_version(kid_s, sw_version_s)

				local pld = { hw_type = e_arg.pld.hw_type }
				pld.sw_version, pld.sw_size, pld.sw_crc = kube_target_sw(hw_type_s)
				reply(e_arg.hdr, FMT_UPGRADE_REPLY, pld)
			end
		},
		download = state {
			entry = function()
				decode("@3 " .. FMT_DOWNLOAD_REQUEST, e_arg)
				local kid_s = tostring(e_arg.hdr.nid)
				if not is_kid_allocated(kid_s) then return end --TODO raise error
				local hw_type = tonumber(KUBE[kid_s].hw_type)
				local sw_version = e_arg.pld.sw_version
				local sw_index = e_arg.pld.sw_index

				local pld = {
					sw_xor = nixio.bit.bxor(sw_version, sw_index),
					sw_block = get_firmware_block(hw_type, sw_version, sw_index)
				}

				if pld.sw_block then
					reply(e_arg.hdr, FMT_DOWNLOAD_REPLY, pld)
				end
			end
		},
		trans { src = "initial", tgt = "receiving" },
		trans { src = "receiving", tgt = "receiving", events = { "e_packet" },
				effect = decode(collect_script, e_arg) }, --TODO map to JSON packet description
		trans { src = "receiving", tgt = "upgrade", events = { "e_upgrade_request" } },
		trans { src = "upgrade", tgt = "receiving", events = { "e_done" } },
		trans { src = "receiving", tgt = "download", events = { "e_download_request" } },
		trans { src = "download", tgt = "receiving", events = { "e_done" } }
	},

	pairing = state {
		entry = function()
				-- TODO listen on pairing grp 212
			end,

		receiving = state { },
		pair_request = state {
			entry = function()
				decode("@3 " .. FMT_PAIR_REQUEST, e_arg)
			end
		},
		pair_reply = state {
			entry = function()
				local kid = get_kid(hex(e_arg.pld.hw_id))

				if kid then
					local pld = {
						hw_type = tonumber(KUBE[tostring(kid)].hw_type),
						grp = tonumber(KUBE.main.collect_group),
						nid = kid,
						key = KUBE.main.key
					}

					reply(e_arg.hdr, FMT_PAIR_REPLY, pld)
				end
			end
		},
		provision = state {
			entry = function()
				provision_kube(hex(e_arg.pld.hw_id), tostring(e_arg.pld.hw_type))
			end
		},

		trans { src = "initial", tgt = "receiving" },
		trans { src = "receiving", tgt = "pair_request", events = { "e_pair_request" } },
		trans { src = "pair_request", tgt = "provision", events = { "e_done" }, pn = 1,
			guard = function()
				return is_not_kube_provisioned(hex(e_arg.pld.hw_id))
			end
		},
		trans { src = "pair_request", tgt = "pair_reply", events = { "e_done" }, pn = 0 },
		trans { src = "provision", tgt = "pair_reply", events = { "e_done" } },
	},

	deprovision = state {
		entry = function()
			deprovision_kube(tostring(e_arg))
		end
	},

	trans { src = "initial", tgt = "collecting" },
	trans { src = "collecting", tgt = "pairing", events = { "e_pair" } },
	trans { src = "pairing", tgt = "collecting", events = { "e_after(30)" } },
	trans { src = ".pairing.pair_reply", tgt = "collecting", events = { "e_done" } },
	trans { src = "collecting", tgt = "deprovision", events = { "e_deprovision" },
		guard = function()
			return type(e_arg) == "number" and is_kid_allocated(tostring(e_arg))
		end
	},
	trans { src = "deprovision", tgt = "collecting", events = { "e_done" } }
}

local fsm = rfsm.init(root)
rfsm.run(fsm)

local function process_event(event, arg)
	e_arg = arg
	if event then rfsm.send_events(fsm, event) end
	rfsm.run(fsm)
end

local ub_methods = {
	["flukso.kube"] = {
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
				process_event("e_deprovision", msg.kid)
				local reply = string.format("kube with kid=%d has been deprovisioned", msg.kid)
				ub:reply(req, { success = true, msg = reply })
			end, { kid = ubus.INT32 }
		}
	}
}

ub:add(ub_methods)

local ub_events = {
	["flukso.kube.event"] = function(msg)
		if type(msg.event) == "string" then
			process_event(msg.event)
		end
	end,

	["flukso.kube.packet.rx"] = function(msg)
		if type(msg.hex) ~= "string" then return end
		local arg = { bin = unhex(msg.hex), hex = msg.hex, hdr = { }, pld = { } }
		vstruct.unpack(FMT_HEADER, arg.bin, arg.hdr)

		if DEBUG.decode then dbg.vardump(arg) end

		local e
		if arg.hdr.ctl and arg.hdr.ack then --OAM packet
			if arg.hdr.len == 22 then
				e = "e_pair_request"
			elseif arg.hdr.len == 8 then
				e = "e_upgrade_request"
			elseif arg.hdr.len == 4 then
				e = "e_download_request"
			end
		else
			e = "e_packet"
		end

		process_event(e, arg)
	end
}

ub:listen(ub_events)

local ut
ut = uloop.timer(function()
		ut:set(ULOOP_TIMEOUT)
		process_event()
	end, ULOOP_TIMEOUT)

uloop:run()
