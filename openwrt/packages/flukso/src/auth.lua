--
-- auth.lua: property and methods for generating hmac-sha1 authentication
-- Copyright (c) 2009 jokamajo.org
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
-- $Id: auth.lua 1 2009-03-13 13:40:00Z icarus75 $
--

require 'luci.model.uci'
require 'lxyssl'

local string, table, os, luci, lxyssl =
      string, table, os, luci, lxyssl

local getfenv, setmetatable, type, pairs, tostring =
      getfenv, setmetatable, type, pairs, tostring

module(...)
local modenv = getfenv()

function new()
  return setmetatable({}, {__index = modenv})
end

function load(T)
  local uci = luci.model.uci.cursor()
  uci:foreach ('system', 'system',
    function(section)
      T.device, T.key, T.version = section.device, section.key, section.version
    end
  )
end

function hmac(T, M, timestamp)
  function string.hex(x)
    local t={}
    for c in x:gmatch('(.)') do t[#t+1]=string.format('%02x', c:byte()) end
    return table.concat(t,'')
  end

  function serialise(M)
    if type(M) == 'table' then
      local sequence = ''
      for k, v in pairs(M) do
        if v ~= nil then 
          sequence = sequence..tostring(k)..serialise(v)
        end
      end
      return sequence
    else
      return tostring(M)
    end
  end

  T.timestamp = timestamp or os.time()
  T.message   = T.timestamp..':'..serialise(M)..':'..T.key -- or T.timestamp..':'..T.nonce..':'..T.key
  T.signature = lxyssl.hash('hmac-sha1', T.key):digest(T.message):hex()

  T.message, T.key, T.version = nil, nil, nil
end