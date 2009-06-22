#!/usr/bin/env lua

--
-- flukso.lua: flukso deamon running on openwrt
-- Copyright (c) 2008-2009 jokamajo.org
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

require 'posix'
require 'xmlrpc.http'

data = require 'flukso.data'
auth = require 'flukso.auth'
dbg  = require 'flukso.dbg'

local param = {xmlrpcaddress = 'http://logger.flukso.net/xmlrpc',
               xmlrpcversion = '1',
               xmlrpcmethod  = 'logger.measurementAdd',
               device        = '/dev/ttyS0',
               interval      = 300}

function receive(device)
  return coroutine.wrap(function()
    -- open the connection to the syslog deamon, specifying our identity
    posix.openlog('flukso')
    posix.syslog(30, 'starting the flukso deamon')
    posix.syslog(30, 'listening for pulses on '..device..'...')

    for line in io.lines(device) do
      if line:sub(1, 3) == 'pls' and line:len() == 47 and line:find(':') == 37 then -- user data + additional data integrity checks
        posix.syslog(30, 'received pulse from '..device..': '..line:sub(5))

        -- flash the power led for 50ms
        os.execute('gpioctl clear 4')
        socket.select(nil, nil, 0.05)
        os.execute('gpioctl set 4')

        local meter, value = line:sub(5, 36), tonumber(line:sub(38))
        coroutine.yield(meter, os.time(), value)
      elseif line:sub(1, 3) == 'msg' then -- control data
        posix.syslog(31, 'received message from '..device..': '..line:sub(5))
      else
        posix.syslog(27, 'input error on '..device..': '..line)
      end
    end

    posix.syslog(30, 'closing down the flukso deamon')
    os.exit()
  end)
end

function buffer(source, interval)
  return coroutine.wrap(function()
    local measurements = data.new()
    local threshold = os.time() + interval

    while true do
      local meter, timestamp, value = source()
      if meter ~= nil and timestamp > 1234567890 then measurements:add(meter, timestamp, value) end
      if timestamp > threshold and next(measurements) then  --checking whether table is not empty
        coroutine.yield(measurements)
        threshold = timestamp + interval
      end
    end
  end)
end

function filter(source, span, offset)
  return coroutine.wrap(function()
    while true do
      local measurements = source()
      measurements:filter(span, offset)
      coroutine.yield(measurements)
    end
  end)
end

function send(source, address, version, method)
  return coroutine.wrap(function()
    while true do
      local measurements = source()
      local auth = auth.new()
      auth:load()
      auth:hmac(measurements)

      local status, ret_or_err, res = pcall(xmlrpc.http.call,
          address..'/'..version,
          method,
          auth,
          measurements)

      if status then
        posix.syslog(30, tostring(res)) 
        if ret_or_err then  --successful xmlrpc call
          measurements:clear()
        end
      else
        posix.syslog(27, tostring(ret_or_err)..' '..address..' '..tostring(res))
      end
      coroutine.yield(measurements)
    end
  end)
end

function gc(source)
  return coroutine.wrap(function()
    while true do
      local measurements = source()
      posix.syslog(31, tostring(collectgarbage('count')*1024)..' bytes of memory used by Lua before garbage collection cycle')
      collectgarbage() -- force a complete garbage collection cycle
      posix.syslog(31, tostring(collectgarbage('count')*1024)..' bytes of memory used by Lua after garbage collection cycle')
      coroutine.yield(measurements)
    end
  end)
end

-- receive: listen to the serial port for incoming pulses
-- buffer: buffer the pulses in a measurement object
-- filter: sweep recursively to filter all redundant entries
-- send: report the measurements to the server via xmlrpc
-- gc: perform a full garbage collection cycle

local aggregator = gc(
                     send(
                       filter(
                         filter(
                           filter(
                             buffer(
                               receive(param.device)
                             , param.interval)
                           , 60, 0)
                         , 900, 7200)
                       , 86400, 172800)
                     , param.xmlrpcaddress, param.xmlrpcversion, param.xmlrpcmethod)
                   )

while true do
  dbg.vardump(aggregator())
end
