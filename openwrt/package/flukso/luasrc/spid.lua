#! /usr/bin/env lua

--[[
    
    spid.lua - Lua part of the Flukso daemon

    Copyright (C) 2011 Bart Van Der Meerssche <bart.vandermeerssche@flukso.net>

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

local dbg   = require 'dbg'
local spi   = require 'flukso.spi'
local nixio = require 'nixio'
nixio.fs    = require 'nixio.fs'
local uci   = require 'luci.model.uci':cursor()

local arg = arg or {} -- needed when this code is not loaded via the interpreter

local DEBUG                 = (arg[1] == '-d')
local MODEL                 = 'FLM02X'
uci:foreach('system', 'system', function(x) MODEL = x.model end)


local SPI_DEV               = '/dev/spidev1.0'
local SPI_MAX_CLK_SPEED_HZ  = 1e6
local SPI_MIN_BYTE_DELAY_US = 300 -- PAVE: Needed to relax the timings even for model B, previous: (MODEL == 'FLM02A') and 250 or 50
local SPI_TX_RX_DELAY_NS    = (MODEL == 'FLM02A') and 2e7 or 5e6
local SPI_CT_DELAY_NS       = 5e8
local POLL_TIMEOUT_MS       = (MODEL == 'FLM02A') and 100 or 50

local UART_MAX_BYTES        = 256

local TIMERFD_ENABLE        = 1
local TIMERFD_SEC           = 1
local TIMERFD_NS            = 0

local GET_DELTA             = 'gd'

local DAEMON                = os.getenv('DAEMON') or 'spid'
local DAEMON_PATH           = os.getenv('DAEMON_PATH') or '/var/run/' .. DAEMON

local O_RDWR_NONBLOCK       = nixio.open_flags('rdwr', 'nonblock')
local POLLIN                = nixio.poll_flags('in')


function mkfifos(input)
	local path = string.format('%s/%s/', DAEMON_PATH, input) 

	nixio.fs.mkdirr(path)
--	nixio.fs.unlink(path .. 'in')  -- clean up mess from previous run
--	nixio.fs.unlink(path .. 'out') -- idem
	nixio.fs.mkfifo(path .. 'in', '644')
	nixio.fs.mkfifo(path .. 'out', '644')

	local fdin  = nixio.open(path .. 'in', O_RDWR_NONBLOCK)
	local fdout = nixio.open(path .. 'out', O_RDWR_NONBLOCK)

	return { fd      = fdin, -- need this entry for nixio.poll
                 fdin    = fdin,
                 fdout   = fdout,
                 events  = POLLIN,
                 revents = 0,
                 line    = fdin:linesource() }
end

local uart  = mkfifos('uart')
local ctrl  = mkfifos('ctrl')
local delta = mkfifos('delta')

if TIMERFD_ENABLE == 1 then
	delta.fd = nixio.timerfd(TIMERFD_SEC, TIMERFD_NS, TIMERFD_SEC, TIMERFD_NS)
end

local fds = { uart, ctrl, delta }

local spidev = nixio.open(SPI_DEV, O_RDWR_NONBLOCK)
nixio.spi.setspeed(spidev, SPI_MAX_CLK_SPEED_HZ, SPI_MIN_BYTE_DELAY_US)
spidev:lock('lock') -- blocks until it can place a write lock on the spidev device

while true do
	local msg
	local poll = nixio.poll(fds, POLL_TIMEOUT_MS)

	if poll == 0 then -- poll timed out, so time to 'poll' the spi bus
		msg = spi.new_msg('', '')
	elseif poll > 0 then
		if ctrl.revents == POLLIN then
			msg = spi.new_msg('ctrl', ctrl.line())
			msg:parse()

		elseif delta.revents == POLLIN then
			if TIMERFD_ENABLE == 1 then
				delta.fd:numexp() -- reset the numexp counter
			else
				delta.line()
			end

			msg = spi.new_msg('delta', GET_DELTA)
			msg:parse()

		elseif uart.revents == POLLIN then
			msg = spi.new_msg('uart', uart.fdin:read(UART_MAX_BYTES))
		end

		msg:encode()
		msg:tx(spidev)
		msg:wait(SPI_TX_RX_DELAY_NS, SPI_CT_DELAY_NS)
	end

	if poll >= 0 then
		msg:rx(spidev)
		local dispatch = msg:decode()
		if DEBUG then dbg.vardump(msg) end

		if dispatch.ctrl then
			ctrl.fdout:write(dispatch.ctrl .. '\n')
		end

		if dispatch.delta then
			delta.fdout:write(dispatch.delta .. '\n')
		end

		if dispatch.uart then
			uart.fdout:write(dispatch.uart)
		end
	end
end
