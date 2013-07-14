#! /usr/bin/env lua

--[[
    
    tic.lua - Flukso télé-info client decoder

    Copyright (C) 2013 Bart Van Der Meerssche <bart@flukso.net>

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

--[[

Structure des trames de la sortie télé-information client

Une trame est constituée de 3 parties :
- le caractère de début de trame "Start Text" STX (02h)
- le corps de la trame, composé d'un ou de plusieurs groupes d'information
- le caractère de fin de trame "End Text" ETX ( 03h)

Une trame peut être interrompue, auquel cas le caractère "End Of Text" EOT
(04h) est transmis avant l'interruption.

Chaque groupe d'information forme un ensemble cohérent avec une étiquette et
une valeur associée. La composition d'un groupe d'information est la suivante:
- le caractère de début de groupe "Line Feed" LF (0Ah)
- le champ étiquette dont la longueur est comprise entre 4 et 8 caractères
- un séparateur "Space" SP (20h)
- le champ « données » dont la longueur est comprise entre 1 et 12 caractères
- un séparateur "Space" SP (20h)
- un champ de contrôle (checksum), composé d'un caractère
- le caractère de fin de groupe "Carriage Return" CR (0Ch)

Le checksum est calculé sur l'ensemble des caractères allant du champ étiquette
à la fin du champ « données », caractère SP inclus. On fait tout d'abord la
somme des codes ASCII de tous ces caractères. Pour éviter d'introduire des
caractères ASCII pouvant être non imprimables, on ne conserve que les six bits
de poids faible du résultat obtenu. Enfin, on ajoute 20h.

]]--

local dbg = require "dbg"
local nixio = require "nixio"
require "nixio.fs"
require "nixio.util"

local string = string
local coroutine = coroutine

local getfenv, setmetatable, tonumber, tostring, pairs, ipairs, print =
      getfenv, setmetatable, tonumber, tostring, pairs, ipairs, print

module (...)
local modenv = getfenv()

--[[
Parsed TIC telegram example:

(table) 
  [PTEC] = (string) HC..
  [HCHC] = (string) 000000018
  [IMAX] = (string) 000
  [ISOUSC] = (string) 45
  [HHPHC] = (string) C
  [MOTDETAT] = (string) 000000
  [HCHP] = (string) 000000061
  [ADCO] = (string) 701328011140
  [IINST] = (string) 000
  [check] = (boolean) true
  [length] = (number) 149
  [PAPP] = (string) 00000
  [OPTARIF] = (string) HC..

]]--

local function object(code, value)
	if telegram then
		telegram[code] = value
	end
end

local function last_object(code, value)
	if telegram then
		telegram[code] = value

		if length then
			if telegram.length == length then
				sync = true
			else
				telegram.check = false

				-- the first telegram might have been corrupted
				if not sync then
					length = telegram.length
				end				
			end
		else
			length = telegram.length
		end

		coroutine.yield(telegram)
	end

	-- fresh table for storing the datagram's COSEM objects
	telegram = {
		length = 0,
		check = true
	}
end

local COSEM = {
	-- CR seems to be removed when followed by a LF
	["^(%u+)%s([%w%.%-]+)%s(.)$"] = object,                    
	["^(%u+)%s([%w%.%-]+)%s(.)%c%c%c$"] = last_object          
}

local function decode(line)
	local match = false

	local function sum(checkstring)
		local checksum = 0

		for i, byte in ipairs({checkstring:byte(1, -1)}) do
			checksum = checksum + byte			
		end

		return (checksum % 0x40) + 0x20
	end

	for pattern, process in pairs(COSEM) do
		local code, value, checksum = line:match(pattern)

		if code then
			if sum(code .. " " .. value) ~= checksum:byte() then
				break
			end

			process(code, value)
			match = true
		end
	end

	if telegram then
		telegram.length = telegram.length + #line

		-- not being able to match a specific line
		-- means we're dealing with a corrupted telegram
		if not match and #line > 0 then
			telegram.check = false
		end
	end
end

function init(dev)
	local O_RDWR = nixio.open_flags("rdwr")

	return coroutine.wrap(
		function()
			local fd = nixio.open(dev, O_RDWR_NONBLOCK)
			local telegram = nil
			local length = nil
			local sync = false

			for line in fd:linesource() do
				decode(line)
			end
		end)
end
