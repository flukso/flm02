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

local getfenv, setmetatable, tonumber, type =
      getfenv, setmetatable, tonumber, type

module (...)
local modenv = getfenv()

local MAX_SENSORS = 6

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

	if(msg.body) then
		msg.parsed.cmd = msg.body:match('^%l%l') or ''
		for arg in msg.body:gmatch('%d+') do
			msg.parsed[#msg.parsed + 1] = tonumber(arg) -- returns nil when string does not contain a number
		end
	end
end

function encode(msg)
	local numtohex = nixio.bin.numtohex

	local noarg_cmd = {
		gd = true,
		gh = true,
		gs = true,
		gp = true,
		gw = true,
		gb = true,
		gk = true,
		ct = true
	}

	local function argcheck(argc)
		if argc ~= #msg.parsed then
			return false
		else
			for i = 1, #msg.parsed do
				if type(msg.parsed[i]) ~= 'number' then
					return false
				end
			end
		end

		return true
	end

	if msg.to == 'uart' then
		msg.encoded = nixio.bin.hexlify(msg.body or '')
		return
	end

	if noarg_cmd[msg.parsed.cmd] then
		msg.encoded = msg.parsed.cmd

	elseif msg.parsed.cmd == 'rt' and argcheck(2) then
		msg.encoded = msg.parsed.cmd .. numtohex(msg.parsed[1], 1)
                                     .. numtohex(msg.parsed[2], 1)

	elseif msg.parsed.cmd == 'ge' and argcheck(1) then
		msg.encoded = msg.parsed.cmd .. numtohex(msg.parsed[1], 1)

	elseif msg.parsed.cmd == 'gc' and argcheck(1) then
		msg.encoded = msg.parsed.cmd .. numtohex(msg.parsed[1], 1)

	elseif msg.parsed.cmd == 'gm' and argcheck(1) then
		msg.encoded = msg.parsed.cmd .. numtohex(msg.parsed[1], 1)

	elseif msg.parsed.cmd == 'gf' and argcheck(1) then
		msg.encoded = msg.parsed.cmd .. numtohex(msg.parsed[1], 1)

	elseif msg.parsed.cmd == 'sh' and argcheck(2) then
		msg.encoded = msg.parsed.cmd .. numtohex(msg.parsed[1], 2)
                                     .. numtohex(msg.parsed[2], 1)

	elseif msg.parsed.cmd == 'ss' and argcheck(2) then
		msg.encoded = msg.parsed.cmd .. numtohex(msg.parsed[1], 1)
                                     .. numtohex(msg.parsed[2], 1)

	elseif msg.parsed.cmd == 'se' and argcheck(2) then
		msg.encoded = msg.parsed.cmd .. numtohex(msg.parsed[1], 1)
                                     .. numtohex(msg.parsed[2], 1)

	elseif msg.parsed.cmd == 'sp' and argcheck(6) then
		msg.encoded = msg.parsed.cmd

		for i = 1, MAX_SENSORS do
			msg.encoded = msg.encoded .. numtohex(msg.parsed[i], 1)
		end

	elseif msg.parsed.cmd == 'sc' and argcheck(2) then
		msg.encoded = msg.parsed.cmd .. numtohex(msg.parsed[1], 1)
                                     .. numtohex(msg.parsed[2], 4)

	elseif msg.parsed.cmd == 'sm' and argcheck(2) then
		msg.encoded = msg.parsed.cmd .. numtohex(msg.parsed[1], 1)
                                     .. numtohex(msg.parsed[2], 2)

	elseif msg.parsed.cmd == 'sf' and argcheck(2) then
		msg.encoded = msg.parsed.cmd .. numtohex(msg.parsed[1], 1)
                                     .. numtohex(msg.parsed[2], 2)

	elseif msg.parsed.cmd == 'sw' and argcheck(1) then
		msg.encoded = msg.parsed.cmd .. numtohex(msg.parsed[1], 2)

	elseif msg.parsed.cmd == 'sb' and argcheck(1) then
		msg.encoded = msg.parsed.cmd .. numtohex(msg.parsed[1], 2)

	elseif msg.parsed.cmd == 'sk' and argcheck(3) then
		msg.encoded = msg.parsed.cmd

		for i = 1, 3 do
			msg.encoded = msg.encoded .. numtohex(msg.parsed[i], 1)
		end

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

function wait(msg, short, long)
	if msg.parsed and msg.parsed.cmd and msg.parsed.cmd == 'ct' then
		nixio.nanosleep(0, long)
	else
		nixio.nanosleep(0, short)
	end
end

function rx(msg, cdev)
	local hextonum = nixio.bin.hextonum

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

		if nixio.bin.dow_crc(msg.received.l) ~= hextonum(msg.received.crc) then
			--> TODO implement near-end crc error counter
			msg.received.l = nil
		end
	end
end

function decode(msg)
	local hextonum = nixio.bin.hextonum

	msg.decoded = {}

	if msg.received.u then
		msg.decoded.uart = nixio.bin.unhexlify(msg.received.u)
	end

	if msg.received.l then
		msg.decoded.cmd  = msg.received.l:sub(1,  2)
		msg.decoded.args = msg.received.l:sub(3, -1)

		if msg.decoded.cmd == 'gd' and msg.decoded.args ~= '' then
			for i = 1, msg.decoded.args:len() / 18 do
				msg.decoded[(i-1)*3 + 1] =
					hextonum(msg.decoded.args:sub((i-1)*18 +  1, (i-1)*18 +  2))
				msg.decoded[(i-1)*3 + 2] =
					hextonum(msg.decoded.args:sub((i-1)*18 +  3, (i-1)*18 + 10))
				msg.decoded[(i-1)*3 + 3] =
					hextonum(msg.decoded.args:sub((i-1)*18 + 11, (i-1)*18 + 18))
			end

			msg.decoded.delta = os.time() .. ' ' .. table.concat(msg.decoded, ' ')

		elseif msg.decoded.cmd == 'rt' then
			msg.decoded[1] = hextonum(msg.decoded.args:sub(1, 2))
			msg.decoded[2] = hextonum(msg.decoded.args:sub(3, 4))

			msg.decoded.ctrl = msg.decoded.cmd .. ' ' .. table.concat(msg.decoded, ' ')

		elseif msg.decoded.cmd == 'gh' or msg.decoded.cmd == 'sh' then
			msg.decoded[1] = hextonum(msg.decoded.args:sub(1, 4))
			msg.decoded[2] = hextonum(msg.decoded.args:sub(5, 6))

			msg.decoded.ctrl = msg.decoded.cmd .. ' ' .. table.concat(msg.decoded, ' ')

		elseif msg.decoded.cmd == 'gs' or msg.decoded.cmd == 'ss' then
			msg.decoded[1] = hextonum(msg.decoded.args:sub(1, 2))
			msg.decoded[2] = hextonum(msg.decoded.args:sub(3, 4))

			msg.decoded.ctrl = msg.decoded.cmd .. ' ' .. table.concat(msg.decoded, ' ')

		elseif msg.decoded.cmd == 'ge' or msg.decoded.cmd == 'se' then
			msg.decoded[1] = hextonum(msg.decoded.args:sub(1, 2))
			msg.decoded[2] = hextonum(msg.decoded.args:sub(3, 4))

			msg.decoded.ctrl = msg.decoded.cmd .. ' ' .. table.concat(msg.decoded, ' ')

		elseif msg.decoded.cmd == 'gp' or msg.decoded.cmd == 'sp' then
			for i = 1, MAX_SENSORS do
				msg.decoded[i] = hextonum(msg.decoded.args:sub(i*2 - 1, i*2))
			end

			msg.decoded.ctrl = msg.decoded.cmd .. ' ' .. table.concat(msg.decoded, ' ')

		elseif msg.decoded.cmd == 'gc' or msg.decoded.cmd == 'sc' then
			msg.decoded[1] = hextonum(msg.decoded.args:sub(1, 2))
			msg.decoded[2] = hextonum(msg.decoded.args:sub(3, 10))

			msg.decoded.ctrl = msg.decoded.cmd .. ' ' .. table.concat(msg.decoded, ' ')

		elseif msg.decoded.cmd == 'gm' or msg.decoded.cmd == 'sm' then
			msg.decoded[1] = hextonum(msg.decoded.args:sub(1, 2))
			msg.decoded[2] = hextonum(msg.decoded.args:sub(3, 6))

			msg.decoded.ctrl = msg.decoded.cmd .. ' ' .. table.concat(msg.decoded, ' ')

		elseif msg.decoded.cmd == 'gf' or msg.decoded.cmd == 'sf' then
			msg.decoded[1] = hextonum(msg.decoded.args:sub(1, 2))
			msg.decoded[2] = hextonum(msg.decoded.args:sub(3, 6))

			msg.decoded.ctrl = msg.decoded.cmd .. ' ' .. table.concat(msg.decoded, ' ')

		elseif msg.decoded.cmd == 'gw' or msg.decoded.cmd == 'sw' then
			msg.decoded[1] = hextonum(msg.decoded.args:sub(1, 4))

			msg.decoded.ctrl = msg.decoded.cmd .. ' ' .. table.concat(msg.decoded, ' ')

		elseif msg.decoded.cmd == 'gb' or msg.decoded.cmd == 'sb' then
			msg.decoded[1] = hextonum(msg.decoded.args:sub(1, 4))

			msg.decoded.ctrl = msg.decoded.cmd .. ' ' .. table.concat(msg.decoded, ' ')

		elseif msg.decoded.cmd == 'gk' or msg.decoded.cmd == 'sk' then
			for i = 1, 3 do
				msg.decoded[i] = hextonum(msg.decoded.args:sub(i*2 - 1, i*2))
			end

			msg.decoded.ctrl = msg.decoded.cmd .. ' ' .. table.concat(msg.decoded, ' ')

		elseif msg.decoded.cmd == 'ct' then
			msg.decoded.ctrl = msg.decoded.cmd

		elseif msg.decoded.cmd == 'zz' then
			--> TODO implement far-end crc error counter
		end
	end

	return { ctrl = msg.decoded.ctrl, delta = msg.decoded.delta, uart = msg.decoded.uart } 
end
