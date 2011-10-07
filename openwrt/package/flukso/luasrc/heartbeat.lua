#!/usr/bin/env lua

--[[
    
    heartbeat.lua - send a heartbeat to the flukso server

    Copyright (C) 2008-2009 jokamajo.org
                  2011 Bart Van Der Meerssche <bart.vandermeerssche@flukso.net>

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

if not arg[1] then
	print ('Please pass the reset argument as a boolean to the script.')
	os.exit(1)
end

local dbg        = require 'dbg'
local nixio      = require 'nixio'
nixio.fs         = require 'nixio.fs'
local uci        = require 'luci.model.uci'.cursor()
local luci       = require 'luci'
luci.sys         = require 'luci.sys'
luci.json        = require 'luci.json'
local httpclient = require 'luci.httpclient'


-- parse and load /etc/config/flukso
local FLUKSO		= uci:get_all('flukso')

-- WAN settings
local WAN_ENABLED	= (FLUKSO.daemon.enable_wan_branch == '1')

local WAN_BASE_URL	= FLUKSO.daemon.wan_base_url .. 'device/'
local WAN_KEY		= '0123456789abcdef0123456789abcdef'
uci:foreach('system', 'system', function(x) WAN_KEY = x.key end) -- quirky but it works

local DEVICE		= '0123456789abcdef0123456789abcdef'
uci:foreach('system', 'system', function(x) DEVICE = x.device end)

local UPGRADE_URL	= FLUKSO.daemon.upgrade_url

-- https header helpers
local FLUKSO_VERSION	= '000'
uci:foreach('system', 'system', function(x) FLUKSO_VERSION = x.version end)

local USER_AGENT	= 'Fluksometer v' .. FLUKSO_VERSION
local CACERT		= FLUKSO.daemon.cacert

-- gzipped syslog tmp file
local SYSLOG_TMP	= '/tmp/syslog.gz'
local SYSLOG_GZIP	= 'logread | gzip > ' .. SYSLOG_TMP

-- collect relevant monitoring points
local function collect_mp()
	local monitor = {}

	monitor.reset = tonumber(arg[1])
	monitor.version = tonumber(FLUKSO_VERSION)
	monitor.time = os.time()
	monitor.uptime  = math.floor(luci.sys.uptime())
	system, model, monitor.memtotal, monitor.memcached, monitor.membuffers, monitor.memfree = luci.sys.sysinfo()

	os.execute(SYSLOG_GZIP)
	io.input(SYSLOG_TMP)
	local syslog_gz = io.read("*all")

	monitor.syslog = nixio.bin.b64encode(syslog_gz)

	return monitor
end

-- terminate when WAN reporting is not set
if not WAN_ENABLED then
	os.exit(2)
end

-- open the connection to the syslog deamon, specifying our identity
nixio.openlog('heartbeat', 'pid')

local monitor = collect_mp()
local monitor_json = luci.json.encode(monitor)


-- phone home
local headers = {}
headers['Content-Type'] = 'application/json'
headers['X-Version'] = '1.0'
headers['User-Agent'] = USER_AGENT
headers['Connection'] = 'close'

local options = {}
options.sndtimeo = 5
options.rcvtimeo = 5
-- We don't enable peer cert verification so we can still update/upgrade
-- the Fluksometer via the heartbeat call even when the cacert has expired.
-- Disabling validation does mean that the server has to include an hmac
-- digest in the reply that the Fluksometer needs to verify, this to prevent
-- man-in-the-middle attacks.
options.tls_context_set_verify = 'none'
-- options.cacert = CACERT
options.method  = 'POST'
options.headers = headers
options.body = luci.json.encode(monitor)

local hash = nixio.crypto.hmac('sha1', WAN_KEY)
hash:update(options.body)
options.headers['X-Digest'] = hash:final()

local http_persist = httpclient.create_persistent()
local url = WAN_BASE_URL .. DEVICE
local response_json, code, call_info = http_persist(url, options)

if code == 200 then
	nixio.syslog('info', string.format('%s %s: %s', options.method, url, code))
else
	nixio.syslog('err', string.format('%s %s: %s', options.method, url, code))

	-- if available, send additional error info to the syslog
	if type(call_info) == 'string' then
		nixio.syslog('err', call_info)
	elseif type(call_info) == 'table'  then
		local auth_error = call_info.headers['WWW-Authenticate']

		if auth_error then
			nixio.syslog('err', string.format('WWW-Authenticate: %s', auth_error))
		end
	end

	os.exit(3)
end

-- verify the reply's digest
hash = nixio.crypto.hmac('sha1', WAN_KEY)
hash:update(response_json)
if call_info.headers['X-Digest'] ~= hash:final() then
	nixio.syslog('err', 'Incorrect digest in the heartbeat reply. Discard response.')
	os.exit(4)
end

local response = luci.json.decode(response_json)

-- check whether we have to reset or upgrade
if response.upgrade == monitor.version then
	os.execute('reboot')
elseif response.upgrade > monitor.version then
	os.execute('wget -q -P /tmp ' .. UPGRADE_URL .. 'upgrade.' .. response.upgrade)
	os.execute('chmod a+x /tmp/upgrade.' .. response.upgrade)
	os.execute('/tmp/upgrade.' .. response.upgrade)
	os.execute('rm /tmp/upgrade.' .. response.upgrade)
end
