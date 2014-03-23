#! /usr/bin/env lua

--[[
    
    spid.lua - spidev dispatcher

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
local spi = require "flukso.spi"
local nixio = require "nixio"
nixio.fs = require "nixio.fs"
local uloop = require "uloop"
uloop.init()
local ubus = require "ubus"
local ub = assert(ubus.connect(), "unable to connect to ubus")
local uci = require "luci.model.uci":cursor()

local arg = arg or {} -- needed when this code is not loaded via the interpreter

local DEBUG                 = (arg[1] == "-d")
local MODEL                 = "FLM02X"
uci:foreach("system", "system", function(x) MODEL = x.model end)

local SPI_DEV               = "/dev/spidev1.0"
local SPI_MAX_CLK_SPEED_HZ  = 1e6
local SPI_MIN_BYTE_DELAY_US = (MODEL == "FLM02A") and 250 or 50
local SPI_TX_RX_DELAY_NS    = (MODEL == "FLM02A") and 2e7 or 5e6
local SPI_CT_DELAY_NS       = 5e8
local POLL_TIMEOUT_MS       = (MODEL == "FLM02A") and 100 or 50
local DELTA_TIMEOUT_MS      = 1e3

local GET_DELTA             = "gd"

local DAEMON                = os.getenv("DAEMON") or "spid"
local DAEMON_PATH           = os.getenv("DAEMON_PATH") or "/var/run/" .. DAEMON

local O_RDWR_NONBLOCK       = nixio.open_flags("rdwr", "nonblock")


function mkfifos(input)
	local path = string.format("%s/%s/", DAEMON_PATH, input) 

	nixio.fs.mkdirr(path)
--	nixio.fs.unlink(path .. "in")  -- clean up mess from previous run
--	nixio.fs.unlink(path .. "out") -- idem
	nixio.fs.mkfifo(path .. "in", "644")
	nixio.fs.mkfifo(path .. "out", "644")

	local fdin  = nixio.open(path .. "in", O_RDWR_NONBLOCK)
	local fdout = nixio.open(path .. "out", O_RDWR_NONBLOCK)

	return {
		fdin = fdin,
		fdout = fdout,
		line = fdin:linesource() }
end

local delta = mkfifos("delta")

local spidev = nixio.open(SPI_DEV, O_RDWR_NONBLOCK)
nixio.spi.setspeed(spidev, SPI_MAX_CLK_SPEED_HZ, SPI_MIN_BYTE_DELAY_US)
spidev:lock("lock") -- blocks until it can place a write lock on the spidev device


local function dispatch(msg, req)
	msg:rx(spidev)
	local decode = msg:decode()
	if DEBUG then dbg.vardump(msg) end

	if decode.ctrl then
		ub:reply(req, { success = true, result = decode.ctrl })
	end

	if decode.delta then
		delta.fdout:write(decode.delta .. "\n")
	end

	if decode.uart then
		ub:send("flukso.kube.packet.rx", { hex = decode.uart })
	end
end

local utimer_poll
utimer_poll = uloop.timer(
	function()
		utimer_poll:set(POLL_TIMEOUT_MS)
		local msg = spi.new_msg("", "") -- poll the spi bus
		dispatch(msg)
	end, POLL_TIMEOUT_MS)

local utimer_delta
utimer_delta = uloop.timer(
	function()
		utimer_delta:set(DELTA_TIMEOUT_MS)
		local msg = spi.new_msg("delta", GET_DELTA)
		msg:parse()
		msg:encode()
		msg:tx(spidev)
		msg:wait(SPI_TX_RX_DELAY_NS, SPI_CT_DELAY_NS)
		dispatch(msg)	
	end, DELTA_TIMEOUT_MS)

local ub_methods = {
	["flukso.flx"] = {
		ctrl = {
			function(req, rpc)
				if type(rpc.cmd) ~= "string" then return end
				local msg = spi.new_msg("ctrl", rpc.cmd)
				msg:parse()
				msg:encode()
				msg:tx(spidev)
				msg:wait(SPI_TX_RX_DELAY_NS, SPI_CT_DELAY_NS)
				dispatch(msg, req)
			end, { cmd = ubus.STRING }
		}
	}
}

ub:add(ub_methods)

local ub_events = {
	["flukso.kube.packet.tx"] = function(pkt)
		if type(pkt.hex) ~= "string" then return end
		local msg = spi.new_msg("uart", pkt.hex)
		msg:encode() --TODO do we still need an encoding step?
		msg:tx(spidev)
		msg:wait(SPI_TX_RX_DELAY_NS, SPI_CT_DELAY_NS)
		dispatch(msg)
	end
}

ub:listen(ub_events)
uloop:run()
