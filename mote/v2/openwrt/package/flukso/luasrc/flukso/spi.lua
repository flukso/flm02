--[[
    
    spi.lua - Lua 5.1 flukso module for spidev message processing

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
local BinDecHex = require 'flukso.BinDecHex'

local os, table, string =
      os, table, string

local getfenv, setmetatable, tonumber =
      getfenv, setmetatable, tonumber

module (...)
local modenv = getfenv()

-- private
local function dow_crc(sequence, crc)
	crc = crc or 0x00

	local r1 = { 0x00, 0x5e, 0xbc, 0xe2, 0x61, 0x3f, 0xdd, 0x83,
                     0xc2, 0x9c, 0x7e, 0x20, 0xa3, 0xfd, 0x1f, 0x41 }

	local r2 = { 0x00, 0x9d, 0x23, 0xbe, 0x46, 0xdb, 0x65, 0xf8,
                     0x8c, 0x11, 0xaf, 0x32, 0xca, 0x57, 0xe9, 0x74 }

	local first_char = sequence:sub(1, 1)

	if first_char ~= '' then
		local i = nixio.bit.band(nixio.bit.bxor(first_char:byte(), crc), 0xff)
		crc = nixio.bit.bxor(r1[nixio.bit.band(i, 0xf) + 1], r2[nixio.bit.rshift(i, 4) + 1])
		return dow_crc(sequence:sub(2, -1), crc)
	else
		return nixio.bin.hexlify(string.char(crc))
	end
end

--- Create a new spi message object.
--
-- Attributes:
-- { to = ctrl | delta | uart
--   body = <string>
--   parsed = { <command>, <arg1>, <arg2>, ... }
--   encoded = <string>
--   received = { raw = <string>, l = <string>, crc = <string>, u = <string> }
--   decoded = { l = ..., u = ... }
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

	msg.parsed[1] = msg.body:match('^%l%l')
	for arg in msg.body:gmatch('%d+') do
		msg.parsed[#msg.parsed + 1] = arg
	end

	--> TODO: more detailed command syntax checking
end

function encode(msg)
	if msg.to == 'uart' then
		msg.encoded = nixio.bin.hexlify(msg.body)
		return
	end

	if msg.parsed[1] == 'gd' then
		msg.encoded = msg.parsed[1]
	elseif msg.parsed[1] == 'gv' then

	elseif msg.parsed[1] == 'gp' then
        
	elseif msg.parsed[1] == 'gc' then
        
	elseif msg.parsed[1] == 'gm' then

	elseif msg.parsed[1] == 'gw' then

	elseif msg.parsed[1] == 'gb' then

	elseif msg.parsed[1] == 'sv' then

	elseif msg.parsed[1] == 'sp' then

	elseif msg.parsed[1] == 'sc' then

	elseif msg.parsed[1] == 'sm' then

	elseif msg.parsed[1] == 'sw' then

	elseif msg.parsed[1] == 'sb' then

	elseif msg.parsed[1] == 'ct' then

	else

	end

--> TODO msg.encoded = msg.encoded .. dow_crc(msg.encoded)
end

function tx(msg, cdev)
	if msg.to == 'ctrl' or msg.to == 'delta' then
		cdev:write('l' .. msg.encoded .. '.\0\0')
	elseif msg.to == 'uart' then
		cdev:write('u' .. msg.encoded .. '\0\0')
	end
end

function rx(msg, cdev)
	local sequence = {}

	for char in function() return cdev:read(1) end do
		if char ~= '\0' then
			table.insert(sequence, char)
		else
        	        -- one more read to let the AVR send a second 0x00
			-- after which the AVR's state machine toggles to read mode
			cdev:read(1)
			break
		end
	end

	msg.received = {}
	msg.received.raw = table.concat(sequence)
	msg.received.l, msg.received.u = msg.received.raw:match('^l(%w*)%.?u(%w*)%.?$')

	if msg.received.l ~= '' and msg.received.l:sub(1, 2) == msg.parsed[1] then
		msg.received.crc = msg.received.l:sub(-2, -1)
		msg.received.l   = msg.received.l:sub(1, -3)

		if dow_crc(msg.received.l) ~= msg.received.crc then
			--> TODO implement crc error counter
			msg.received.l = ''
		end
	end
end

function decode(msg)
	msg.decoded = {}

	if msg.received.u ~= '' then
		msg.decoded.u = nixio.bin.unhexlify(msg.received.u)
	end

	if msg.received.l ~= '' then
		msg.decoded.largs = msg.received.l:sub(3, -1)

		if msg.parsed[1] == 'gd' then
			for i = 1, msg.decoded.largs:len() / 18 do
				msg.decoded[(i-1)*3 + 1] =
					tonumber(BinDecHex.Hex2Dec(msg.decoded.largs:sub((i-1)*18 +  1, (i-1)*18 +  2)))
				msg.decoded[(i-1)*3 + 2] =
					tonumber(BinDecHex.Hex2Dec(msg.decoded.largs:sub((i-1)*18 +  3, (i-1)*18 + 10)))
				msg.decoded[(i-1)*3 + 3] =
					tonumber(BinDecHex.Hex2Dec(msg.decoded.largs:sub((i-1)*18 + 11, (i-1)*18 + 18)))
			end
		elseif msg.parsed[1] == 'gv' then

		elseif msg.parsed[1] == 'gp' then

		elseif msg.parsed[1] == 'gc' then

		elseif msg.parsed[1] == 'gm' then

		elseif msg.parsed[1] == 'gw' then

		elseif msg.parsed[1] == 'gb' then

		elseif msg.parsed[1] == 'sv' then

		elseif msg.parsed[1] == 'sp' then

		elseif msg.parsed[1] == 'sc' then

		elseif msg.parsed[1] == 'sm' then

		elseif msg.parsed[1] == 'sw' then

		elseif msg.parsed[1] == 'sb' then

		elseif msg.parsed[1] == 'ct' then

		end
	end
end
