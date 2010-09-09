#!/usr/bin/env lua

--
-- flukso.lua: flukso deamon running on openwrt
-- Copyright (c) 2008-2009 jokamajo.org
--               2010      flukso.net
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
               pwrenable     = true,
               pwrinterval   = 0,
               pwrdir        = '/tmp/sensor',
               device        = '/dev/ttyS0',
               interval      = 300,
               dbgenable     = false}

function dispatch(e_child, p_child, device, pwrenable)
  return coroutine.create(function()

    local function flash()  -- flash the power led for 50ms
      os.execute('gpioctl clear 4 > /dev/null')
      socket.select(nil, nil, 0.05)
      os.execute('gpioctl set 4 > /dev/null')
    end

    -- open the connection to the syslog deamon, specifying our identity
    posix.openlog('flukso')
    posix.syslog(30, 'starting the flukso deamon')
    posix.syslog(30, 'listening for pulses on '..device..'...')

    local pattern = '^(%l+)%s(%x+):(%d+):?(%d*)$'

    for line in io.lines(device) do
      local command, meter, value, msec = line:match(pattern)
      value = tonumber(value or '0')
      msec = tonumber(msec or '0')
      local length = line:len()

      if command == 'pls' and (length == 47 or length == 58) then  -- user data
        flash()
        posix.syslog(30, 'received pulse from ' .. device .. ': ' .. line:sub(5))

        coroutine.resume(e_child, meter, os.time(), value)

        -- pls includes a msec timestamp so report to p_child as well
        if length == 58 then
          coroutine.resume(p_child, meter, os.time(), value, msec)
        end

      elseif command == 'pwr' and length == 47 then                 -- user data
        if pwrenable then coroutine.resume(p_child, meter, os.time(), value) end

      elseif command == 'msg' then                                  -- control data
        posix.syslog(31, 'received message from ' .. device .. ': ' .. line:sub(5))

      else                                                          -- error
        posix.syslog(27, 'input error on ' .. device .. ': ' .. line)
      end
    end

    posix.syslog(30, 'closing down the flukso deamon')
    os.exit(1)
  end)
end

function buffer(child, interval)
  return coroutine.create(function(meter, timestamp, value, msec)
    local measurements = data.new()
    local threshold = timestamp + interval
    local prev = {}

    local function diff(x, y)  -- calculates y - x
      if y >= x then
        return y - x
      else -- y wrapped around 32-bit boundary
        return (0xFFFFFFFF - x) + y + 1
      end
    end
    
    while true do
      if meter ~= nil then
        if not prev[meter] then
          prev[meter] = {}
        end

        if msec then  -- we're dealing with a pls xxx:yyy:zzz message so calculate power
          if prev[meter].msec then
            local power = math.floor(diff(prev[meter].value, value) / diff(prev[meter].msec, msec) * 3.6 * 10^6 + 0.5)
            prev[meter].value = value
            value = power
          else
            prev[meter].value = value
            value = nil
          end
          prev[meter].msec = msec
        end

        if timestamp > math.max(1234567890, prev[meter].timestamp or 0) and value then
          measurements:add(meter, timestamp, value)
        end
      end

      if timestamp > threshold and next(measurements) then  --checking whether table is not empty
        coroutine.resume(child, measurements)
        threshold = timestamp + interval
        prev[meter].timestamp = timestamp
      end
      meter, timestamp, value, msec = coroutine.yield()
    end
  end)
end

function filter(child, span, offset)
  return coroutine.create(function(measurements)
    while true do
      measurements:filter(span, offset)
      coroutine.resume(child, measurements)
      measurements = coroutine.yield()
    end
  end)
end

function send(child, address, version, method)
  return coroutine.create(function(measurements)
    while true do
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
      coroutine.resume(child, measurements)
      measurements = coroutine.yield()
    end
  end)
end

function gc(child)
  return coroutine.create(function(measurements)
    while true do
      posix.syslog(31, tostring(collectgarbage('count')*1024)..' bytes of memory used by Lua before garbage collection cycle')
      collectgarbage() -- force a complete garbage collection cycle
      posix.syslog(31, tostring(collectgarbage('count')*1024)..' bytes of memory used by Lua after garbage collection cycle')
      coroutine.resume(child, measurements)
      measurements = coroutine.yield()
    end
  end)
end

function polish(child, cutoff)
  return coroutine.create(function(measurements)
    while true do
      measurements:fill()
      measurements:truncate(cutoff)
      coroutine.resume(child, measurements)
      measurements = coroutine.yield()
    end
  end)
end

function publish(child, dir)
  return coroutine.create(function(measurements)
    os.execute('mkdir -p ' .. dir .. ' > /dev/null')
    while true do
      local measurements_json = measurements:json_encode()
      for meter, json in pairs(measurements_json) do
        io.output(dir .. '/' .. meter)
        io.write(json)
        io.close()
      end
      coroutine.resume(child, measurements)
      measurements = coroutine.yield()
    end
  end)
end

function debug(child, dbgenable)
  return coroutine.create(function(measurements)
    while true do
      if dbgenable then dbg.vardump(measurements) end
      if child then coroutine.resume(child, measurements) end
      measurements = coroutine.yield()
    end
  end)
end

-- dispatch: listen to the serial port for incoming pulses
-- buffer: buffer the pulses in a measurement object
-- filter: sweep recursively to filter all redundant entries
-- send: report the measurements to the server via xmlrpc
-- gc: perform a full garbage collection cycle
-- debug: dump measurements table to stdout

local e_chain = buffer(
                  filter(
                    filter(
                      filter(
                        send(
                          gc(
                            debug(nil, param.dbgenable)
                          )
                        , param.xmlrpcaddress, param.xmlrpcversion, param.xmlrpcmethod)
                      , 86400, 172800)
                    , 900, 7200)
                  , 60, 0)
                , param.interval)

local p_chain = buffer(
                  polish(
                    publish(
                      debug(nil, param.dbgenable)
                    , param.pwrdir)
                  , 60)
                , param.pwrinterval)

local chain = dispatch(e_chain, p_chain, param.device, param.pwrenable)

coroutine.resume(chain)
