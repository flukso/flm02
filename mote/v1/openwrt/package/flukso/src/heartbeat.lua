#!/usr/bin/env lua

--
-- Lua 5.1 heartbeat script running on openwrt
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

if not arg[1] then
  print ('Please pass the reset argument as a boolean to the script.')
else
  -- load libraries
  require 'posix'
  require 'xmlrpc.http'
  require 'luci.sys'

  local auth    = require 'flukso.auth'
  local dbg     = require 'flukso.dbg'

  local uci     = require 'luci.model.uci'.cursor()
  local param   = uci:get_all('flukso', 'main')
  local monitor = {reset = tonumber(arg[1])}

  -- open the connection to the syslog deamon, specifying our identity
  posix.openlog('heartbeat')

  -- calculate hmac and collect relevant monitoring points
  local auth = auth.new()
  auth:load()
  monitor.version = tonumber(auth.version)

  monitor.uptime  = math.floor(luci.sys.uptime())
  monitor.uart_oe = string.match(luci.sys.exec('cat /proc/tty/driver/serial'), 'oe:(%d+)') or 0
  system, model, monitor.memtotal, monitor.memcached, monitor.membuffers, monitor.memfree = luci.sys.sysinfo()

  auth:hmac(monitor)

  dbg.vardump(auth)
  dbg.vardump(monitor)

  -- send a heartbeat method call
  local url = 'http://' ..  param.home .. '/xmlrpc/' .. param.homeVersion

  local pcall_ok, return_or_err, pong = pcall(xmlrpc.http.call,
    url,
    'logger.heartbeat',
    auth,
    monitor)

  dbg.vardump(pong)

  if pcall_ok and return_or_err then
    auth:load()
    auth:hmac(pong.upgrade, pong.timestamp)

    if auth.signature == pong.signature and pong.timestamp > os.time() - 300 then
      posix.syslog(31, 'successful heartbeat authentication')

      if tonumber(pong.upgrade) == monitor.version then --reset device
        os.execute('reboot')
      elseif tonumber(pong.upgrade) > monitor.version then -- upgrade device to specified version
        os.execute('wget -P /tmp http://'.. param.home ..'/files/upgrade/upgrade.'..pong.upgrade)
        os.execute('chmod a+x /tmp/upgrade.'..pong.upgrade)
        os.execute('/tmp/upgrade.'..pong.upgrade)
        os.execute('rm /tmp/upgrade.'..pong.upgrade)
      end
    end
  else
    posix.syslog(11, tostring(return_or_err))
  end
  -- close the connection to the syslog deamon
  posix.closelog()
end
