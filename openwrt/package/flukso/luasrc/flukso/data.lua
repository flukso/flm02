--[[
    
    data.lua: property and methods for manipulating incoming measurements

    Copyright (c) 2009 jokamajo.org
           2010 - 2011 Bart Van Der Meerssche <bart.vandermeerssche@flukso.net>

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


local os, math, table, string =
      os, math, table, string

local getfenv, setmetatable, pairs, ipairs =
      getfenv, setmetatable, pairs, ipairs

module (...)
local modenv = getfenv() -- module environment

-- private
local function timestamps(T)
	local H = {} -- helper table, an indexed array containing all the measurement's timestamps
	for timestamp in pairs(T) do
		H[#H+1] = timestamp
	end

	table.sort(H) -- sort in ascending order, oldest timestamps will be treated first
	return H
end

function new()
	return setmetatable({}, {__index = modenv})
end

function add(M, sensor, timestamp, value)
	if not M[sensor] then
		M[sensor] = {}
	end

	M[sensor][timestamp] = value
end

function get_sensors(M)
	local S = {}

	for sensor in pairs(M) do
		S[#S+1] = sensor
	end

	return S
end

function clear(M, sensor)
	if sensor then
		M[sensor] = nil
	else -- clear all
		for sensor in pairs(M) do
			M[sensor] = nil
		end
	end
end

function filter(M, span, offset)
	for sensor, T in pairs(M) do
		local H = timestamps(T)
		local i = 2
		while not (H[i+1] == nil or H[i] > os.time()-offset) do
			if math.floor(H[i-1]/span) == math.floor(H[i]/span) and
                           math.floor(H[i]/span) == math.floor(H[i+1]/span) then
				T[H[i]] = nil
				table.remove(H, i)

			else
				i = i+1
			end
		end
	end
end

function polish(M, now, cutoff)
	for sensor, T in pairs(M) do
		local H = timestamps(T)

		-- fill up the holes first
		for i = H[#H]-1, H[1]+1, -1 do
			if T[i] == nil or T[i] == '"nan"' then
				T[i] = T[i+1]
			end
		end

		-- make sure the tail stretches up to 'now'
		for i = H[#H]+1, now do
			T[i] = '"nan"'
		end

		-- truncate the head
		for i = H[1], now - cutoff do
			T[i] = nil
		end
	end
end

function json_encode(M, entries)
	local J = {}

	if entries then
		arr_size = 4*entries + 1
	else
		arr_size = 0
	end

	for sensor, T in pairs(M) do
		local H = timestamps(T)

		-- use a string buffer for building up the JSON string
		local SB = table.create(arr_size, 0)                                              
		SB[1] = '[['

		for k, timestamp in ipairs(H) do
			SB[#SB+1] = timestamp
			SB[#SB+1] = ','
			SB[#SB+1] = T[timestamp]
			SB[#SB+1] = '],['
		end

		SB[#SB] = ']]'

		J[sensor] = table.concat(SB)
	end

	return J
end
