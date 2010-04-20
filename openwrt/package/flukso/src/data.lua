--
-- data.lua: property and methods for manipulating incoming measurements
-- Copyright (c) 2009 jokamajo.org
--               2010 flukso.net
--
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation; either version 2
-- of the License, or (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
--
-- $Id$
--

local os, math, table, string =
      os, math, table, string

local getfenv, setmetatable, pairs, ipairs =
      getfenv, setmetatable, pairs, ipairs

module (...)
local modenv = getfenv() -- module environment

function new()
  return setmetatable({}, {__index = modenv})
end

function add(M, meter, timestamp, value)
  if not M[meter] then M[meter] = {} end
  M[meter][timestamp] = value
end

function clear(M)
  for meter in pairs(M) do
    M[meter] = nil
  end
end

function filter(M, span, offset)
  for meter, T in pairs(M) do
    local H = timestamps(T)
    local i = 2
    while not (H[i+1] == nil or H[i] > os.time()-offset) do
      if math.floor(H[i-1]/span) == math.floor(H[i]/span) and math.floor(H[i]/span) == math.floor(H[i+1]/span) then
        T[H[i]] = nil
        table.remove(H, i)
      else
        i = i+1
      end
    end
  end
end

function truncate(M, cutoff)
  for meter, T in pairs(M) do
    local H = timestamps(T)
    for i = H[1], H[#H]-60 do
      T[i] = nil
    end
  end
end

function fill(M)
  for meter, T in pairs(M) do
    local H = timestamps(T)
    for i = H[1]+1, H[#H]-1 do
      if T[i] == nil then T[i] = T[i-1] end
    end
  end
end

function json_encode(M)
  local J = {}
  for meter, T in pairs(M) do
    J[meter] = '['
    local H = timestamps(T)
    for k, timestamp in ipairs(H) do
      J[meter] = J[meter] .. '[' .. timestamp .. ',' .. T[timestamp]  .. '],'
    end
    J[meter] = string.sub(J[meter], 1, -2) .. ']'
  end
  return J
end

function timestamps(T)
  local H = {} -- helper table, an indexed array containing all the measurement's timestamps
  for timestamp in pairs(T) do H[#H+1] = timestamp end
  table.sort(H) -- sort in ascending order, oldest timestamps will be treated first
  return H
end
