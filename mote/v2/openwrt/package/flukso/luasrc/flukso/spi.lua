--[[
    
    spi.lua - Lua 5.1 flukso module for spidev message processing and dispatching

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

local nixio = require 'nixio'

local os, table, string =
      os, table, string

local getfenv, setmetatable =
      getfenv, setmetatable

module (...)
local modenv = getfenv()

local SPI_END_OF_MESSAGE	= '.'
local SPI_FORWARD_TO_UART_PORT	= 'u'
local SPI_FORWARD_TO_CTRL_PORT	= 'l' -- 'l'ocal port

local SPI_MAX_READ_BYTES = 1024

--- Create a new spi message object.
--
-- Attributes:
-- { to = ctrl | delta | uart
--   body = <string>
--   parsed = { 'cmd' = <command>, 1 = <arg1>, 2 = <arg2>, ... }
--   encoded = <string>
--   received = { raw = <string>, l = <string>, crc = <string>, u = <string> }
--   decoded = { args = <string>, cmd = <string>, 1 = <arg1>, 2= <arg2>, ..., u = <string> }
--   reply = <string>
-- }
--
-- @param  to   The message destination: ctrl | delta | uart.
-- @param  body The message body.
-- @return An spi message object.
function new_msg(to, body)
	return setmetatable({ to = to, body = body }, { __index = modenv })
end

function parse(msg)
	msg.parsed = {}

	msg.parsed.cmd = msg.body:match('^%l%l')
	for arg in msg.body:gmatch('%d+') do
		msg.parsed[#msg.parsed + 1] = arg
	end

	--> TODO: more detailed command syntax checking
end

function encode(msg)
	if msg.to == 'uart' then
		msg.encoded = nixio.bin.hexlify(msg.body or '')
		return
	end

	if msg.parsed.cmd == 'gd' then
		msg.encoded = msg.parsed.cmd
	elseif msg.parsed.cmd == 'gv' then

	elseif msg.parsed.cmd == 'gp' then
        
	elseif msg.parsed.cmd == 'gc' then
        
	elseif msg.parsed.cmd == 'gm' then

	elseif msg.parsed.cmd == 'gw' then

	elseif msg.parsed.cmd == 'gb' then

	elseif msg.parsed.cmd == 'sv' then

	elseif msg.parsed.cmd == 'sp' then

	elseif msg.parsed.cmd == 'sc' then

	elseif msg.parsed.cmd == 'sm' then

	elseif msg.parsed.cmd == 'sw' then

	elseif msg.parsed.cmd == 'sb' then

	elseif msg.parsed.cmd == 'ct' then

	else
		return
	end

	msg.encoded = msg.encoded .. nixio.bin.numtohex(nixio.bin.dow_crc(msg.encoded), 1)
end

function tx(msg, cdev)
	if msg.encoded then
		if (msg.to == 'ctrl' or msg.to == 'delta') then
			cdev:write(SPI_FORWARD_TO_CTRL_PORT .. msg.encoded .. SPI_END_OF_MESSAGE)
		elseif msg.to == 'uart' then
			cdev:write(SPI_FORWARD_TO_UART_PORT .. msg.encoded)
		end
	end
end

function rx(msg, cdev)
	msg.received = {}
	msg.received.raw = cdev:read(SPI_MAX_READ_BYTES)

	if msg.received.raw == '' then  -- state machine not synced
		msg.received.raw = cdev:read(SPI_MAX_READ_BYTES)
	end

	msg.received.l, msg.received.u = msg.received.raw:match('^l(%w*)%.?u(%w*)%.?$')
	if msg.received.l == '' then msg.received.l = nil end
	if msg.received.u == '' then msg.received.u = nil end

	if msg.received.l then
		msg.received.crc = msg.received.l:sub(-2, -1)
		msg.received.l   = msg.received.l:sub( 1, -3)

		if nixio.bin.dow_crc(msg.received.l) ~= nixio.bin.hextonum(msg.received.crc) then
			--> TODO implement near-end crc error counter
			msg.received.l = nil
		end
	end
end

function decode(msg)
	msg.decoded = {}

	if msg.received.u then
		msg.decoded.uart = nixio.bin.unhexlify(msg.received.u)
	end

	if msg.received.l then
		msg.decoded.cmd  = msg.received.l:sub(1,  2)
		msg.decoded.args = msg.received.l:sub(3, -1)

		if msg.decoded.cmd == 'gd' then
			for i = 1, msg.decoded.args:len() / 18 do
				msg.decoded[(i-1)*3 + 1] =
					nixio.bin.hextonum(msg.decoded.args:sub((i-1)*18 +  1, (i-1)*18 +  2))
				msg.decoded[(i-1)*3 + 2] =
					nixio.bin.hextonum(msg.decoded.args:sub((i-1)*18 +  3, (i-1)*18 + 10))
				msg.decoded[(i-1)*3 + 3] =
					nixio.bin.hextonum(msg.decoded.args:sub((i-1)*18 + 11, (i-1)*18 + 18))
			end

			msg.decoded.delta = os.time() .. ' ' .. table.concat(msg.decoded, ' ')
		elseif msg.decoded.cmd == 'gv' then

		elseif msg.decoded.cmd == 'gp' then

		elseif msg.decoded.cmd == 'gc' then

		elseif msg.decoded.cmd == 'gm' then

		elseif msg.decoded.cmd == 'gw' then

		elseif msg.decoded.cmd == 'gb' then

		elseif msg.decoded.cmd == 'sv' then

		elseif msg.decoded.cmd == 'sp' then

		elseif msg.decoded.cmd == 'sc' then

		elseif msg.decoded.cmd == 'sm' then

		elseif msg.decoded.cmd == 'sw' then

		elseif msg.decoded.cmd == 'sb' then

		elseif msg.decoded.cmd == 'ct' then

		elseif msg.decoded.cmd == 'zz' then
			--> TODO implement far-end crc error counter
		end
	end

	return { ctrl = msg.decoded.ctrl, delta = msg.decoded.delta, uart = msg.decoded.uart } 
end
