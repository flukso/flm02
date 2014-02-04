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
local ubus = require "ubus"
local uloop = require "uloop"
local vstruct = require "vstruct"
vstruct.cache = true
local rfsm = require "rfsm"
rfsm.pp = require "rfsm.pp"
rfsm.timeevent = require "rfsm.timeevent"

local state, trans, conn = rfsm.state, rfsm.trans, rfsm.conn

local ULOOP_TIMEOUT = 1000 --ms

local FMT_HEADER = "< grp:u1 [1| ctl:b1 dst:b1 ack:b1 nid:u5] len:u1"
local FMT_PAIR_REQUEST = "< @3 type:u2 grp:u1 nid:u1 check:u2 hw_id:s16"
local FMT_PAIR_REPLY = "" --TODO

local DEBUG = {
	decode = false,
	encode = false
}

local function rfsm_gettime()
	local secs, usecs = nixio.gettimeofday()
	return secs, usecs * 1e3
end

rfsm.timeevent.set_gettime_hook(rfsm_gettime)

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

local function provision_kube() --TODO placeholder
end

local pkt_buffer

local root = state {
	dbg = false,

	collecting = state {
		entry = function()
				--TODO listen on collecting grp
			end,

		-- rFSM does not support internal transitions
		-- resorting to a nested state without entry/exit actions as a workaround
		decoding = state { },

		trans { src = "initial", tgt = "decoding" },
		trans { src = "decoding", tgt = "decoding", events = { "e_packet" },
				effect = decode(collect_script, pkt_buffer) }, --TODO map to JSON packet description
	},

	pairing = state {
		entry = function()
				-- TODO listen on pairing grp 212
			end,

		decoding = state { },
		pair_request = state {
			entry = function()
				-- TODO figure out the state machine for provisioning
				-- nid and grp == 0 -> fresh
				-- else -> check hwId entry, if exists -> reply with config, else -> re-pair!
				decode(FMT_PAIR_REQUEST, pkt_buffer)
			end
		},
		pair_reply = state {
			entry = function()
				local pld = { } --TODO fill in the blanks
				reply(pkt_buffer.hdr, FMT_PAIR_REPLY, pld)
			end
		},
		provision = state {
			entry = function()
				provision_kube() --TODO define provisioning params
			end
		},

		trans { src = "initial", tgt = "decoding" },
		trans { src = "decoding", tgt = "pair_request", events = { "e_packet_oam" },
			guard = function()
				return pkt_buffer and pkt_buffer.head.len == 22
			end
		},
		trans { src = "pair_request", tgt = "provision", events = { "e_done" }, pn = 1,
			guard = function()
				return true --TODO check for hw_id match in the kube uci file
			end
		},
		trans { src = "pair_request", tgt = "pair_reply", events = { "e_done" }, pn = 0 },
		trans { src = "provision", tgt = "pair_reply", events = { "e_done" } },
	},

	trans { src = "initial", tgt = "collecting" },
	trans { src = "collecting", tgt = "pairing", events = { "e_pair" } },
	trans { src = "pairing", tgt = "collecting", events = { "e_after(30)" } },
	trans { src = ".pairing.pair_reply", tgt = "collecting", events = { "e_done" } },
}

local fsm = rfsm.init(root)
rfsm.run(fsm)

uloop.init()
local ub = assert(ubus.connect(), "unable to connect to ubus")

local ub_methods = {
	["flukso.kubed.debug"] = {
		set = {
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

				if type(msg.decode) == "boolean" then
					DEBUG.decode = msg.decode
				end

				if type(msg.encode) == "boolean" then
					DEBUG.encode = msg.encode
				end

				ub:reply(req, {message = "kubed debugging flags updated"})
			end, { fsm = ubus.BOOLEAN, decode = ubus.BOOLEAN, encode = ubus.BOOLEAN }
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
		pkt_buffer = { bin = nixio.bin.unhexlify(msg.hex), head = { }, pld = { } }
		vstruct.unpack(FMT_HEADER, pkt_buffer.bin, pkt_buffer.head)

		if DEBUG.decode then dbg.vardump(pkt_buffer) end

		local event
		if pkt_buffer.head.ctl and pkt_buffer.head.ack then
			event = "e_packet_oam"
		else
			event = "e_packet"
		end

        rfsm.send_events(fsm, event)
        rfsm.run(fsm)
		pkt_buffer = nil
	end
}

ub:listen(ub_events)

local ut
ut = uloop.timer(function()
		ut:set(ULOOP_TIMEOUT)
		rfsm.run(fsm)
	end, ULOOP_TIMEOUT)

uloop:run()
