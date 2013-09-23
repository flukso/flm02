#!/usr/bin/env lua

--[[
    
    fsync.lua - synchronize /etc/config/flukso settings with the sensor board
                via the spid ctrl fifos

    Copyright (C) 2011-2012 Bart Van Der Meerssche <bart.vandermeerssche@flukso.net>

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


local dbg 			= require 'dbg'
local nixio			= require 'nixio'
nixio.fs			= require 'nixio.fs'
local uci			= require 'luci.model.uci'.cursor()
local luci			= require 'luci'
luci.json			= require 'luci.json'
local httpclient	= require 'luci.httpclient'


local HW_CHECK_OVERRIDE = (arg[1] == '-f')

local CTRL_PATH		= '/var/run/spid/ctrl'
local CTRL_PATH_IN	= CTRL_PATH .. '/in'
local CTRL_PATH_OUT	= CTRL_PATH .. '/out'

local O_RDWR_NONBLOCK	= nixio.open_flags('rdwr', 'nonblock')
local O_RDWR_CREAT		= nixio.open_flags('rdwr', 'creat')
local POLLIN			= nixio.poll_flags('in')
local POLL_TIMEOUT_MS	= 1000
local MAX_TRIES			= 5

-- parse and load /etc/config/flukso
local flukso = uci:get_all('flukso')

local UART_TX_INVERT		= tonumber(flukso.main.uart_tx_invert)
local UART_RX_INVERT		= tonumber(flukso.main.uart_rx_invert)
local MAX_SENSORS			= tonumber(flukso.main.max_sensors)
local MAX_PROV_SENSORS		= tonumber(flukso.main.max_provisioned_sensors)
local MAX_ANALOG_SENSORS	= tonumber(flukso.main.max_analog_sensors)
local ANALOG_ENABLE			= (MAX_ANALOG_SENSORS == 3) and 1 or 0
local RESET_COUNTERS		= (flukso.main.reset_counters == '1')
local WAN_ENABLED			= (flukso.daemon.enable_wan_branch == '1')
local LAN_ENABLED			= (flukso.daemon.enable_lan_branch == '1')

local function last_prov_sensor()
	for i = MAX_PROV_SENSORS, MAX_ANALOG_SENSORS, -1 do
		if flukso[tostring(i)].enable then
			return i
		end
	end
end

local LAST_PROV_SENSOR      = last_prov_sensor()
local MODEL                 = 'FLM02X'
uci:foreach('system', 'system', function(x) MODEL = x.model end)

local METERCONST_FACTOR	= 0.449

-- sensor board commands
local GET_HW_VERSION	= 'gh'
local GET_HW_VERSION_R	= '^gh%s+(%d+)%s+(%d+)$'
local SET_ENABLE		= 'se %d %d'
local SET_HW_LINES		= 'sk %d %d %d' -- ANALOG_EN, UART_RX_INV, UART_TX_INV
local SET_PHY_TO_LOG	= 'sp' -- with [1..MAX_SENSORS] arguments
local SET_METERCONST	= 'sm %d %d'
local SET_FRACTION		= 'sf %d %d'
local SET_COUNTER		= 'sc %d %d'
local COMMIT			= 'ct'

-- LAN settings
local API_PATH		= '/www/sensor/'
local CGI_SCRIPT	= '/usr/bin/restful'
local AVAHI_PATH	= '/etc/avahi/services/flukso.service'

-- WAN settings
local WAN_BASE_URL	= flukso.daemon.wan_base_url .. 'sensor/'
local WAN_KEY		= '0123456789abcdef0123456789abcdef'
uci:foreach('system', 'system', function(x) WAN_KEY = x.key end) -- quirky but it works

-- https header helpers
local FLUKSO_VERSION = '000'
uci:foreach('system', 'system', function(x) FLUKSO_VERSION = x.version end)

local USER_AGENT	= 'Fluksometer v' .. FLUKSO_VERSION
local CACERT		= flukso.daemon.cacert

-- map exit codes to strings
local EXIT_STRING = {
	[-1] = "no synchronisation",
	 [0] = "successful",
	 [1] = "unable to open ctrl fifos",
	 [2] = "detected lock on ctrl fifos",
	 [3] = "synchronisation with sensor board failed",
	 [4] = "sensor board hardware compatibility check failed",
	 [5] = "analog sensor numbering error",
	 [6] = "port numbering error",
	 [7] = "synchronisation with Flukso server failed"
}


--- Convert from Lua-style to c-style index.
-- @param index		Lua-style index startng at 1
-- @return 		C-style index starting at 0 
local function toc(index)
	return index - 1
end

--- Log exit status to syslog first, then exit.
-- @param code  	exit status code
-- @return		none 
local function exit(code)
	local level

	if code == 0 then
		level = 'info'
	else
		level = 'err'
	end

	nixio.syslog(level, string.format('fsync exit status: %d, %s', code, EXIT_STRING[code]))

	uci:set("flukso", "fsync", "time", os.time() - 15)
	uci:set("flukso", "fsync", "exit_status", code)
	uci:set("flukso", "fsync", "exit_string", EXIT_STRING[code])
	uci:commit("flukso")

	os.exit(code)
end

--- Create a pair of file descriptors [fd] to the spid control fifo's.
-- @return		ctrl object containing the fd's, a line-based iterator and poll flags 
local function ctrl_init()
	local ctrl = { fdin    = nixio.open(CTRL_PATH_IN, O_RDWR_NONBLOCK),
                       fdout   = nixio.open(CTRL_PATH_OUT, O_RDWR_NONBLOCK),
                       events  = POLLIN,
                       revents = 0 }

	if ctrl.fdin == nil or ctrl.fdout == nil then
		print('Error. Unable to open the ctrl fifos.')
		print('Exiting...')
		exit(1)
	end

	-- acquire an exclusive lock on the ctrl fifos or exit
	if not (ctrl.fdin:lock('tlock') and ctrl.fdout:lock('tlock')) then
		print('Error. Detected a lock on one of the ctrl fifos.')
		print('Exiting...')
		exit(2)
	end

	ctrl.fd = ctrl.fdout -- need this entry for nixio.poll
	ctrl.line = ctrl.fdout:linesource()

	return ctrl
end

--- Close the spid control fifo's.
-- @param code  	ctrl object
-- @return		none 
local function ctrl_close(ctrl)
	ctrl.fdin:close()
	ctrl.fdout:close()
end

--- Send a command to the control fifo.
-- @param ctrl  	ctrl object
-- @param cmd		command to send
-- @return		none 
local function send(ctrl, cmd)
	while ctrl.line() do end -- flush the out fifo

	for i = 1, MAX_TRIES do
		ctrl.fdin:write(cmd .. '\n')

		local poll, errno, errmsg = nixio.poll({ ctrl }, POLL_TIMEOUT_MS)

		if poll < 0 then
			print('Error. Poll failed with error message: ' .. errmsg)

		elseif poll == 0 then
			print('Error. Poll timed out after ' .. POLL_TIMEOUT_MS .. 'ms')

		elseif poll > 0 then
			reply = ctrl.line()

			if cmd:sub(1, 1) == 's' then
				if reply == cmd then
					print(reply .. ' .. ok')
					return reply
				else
					print(reply .. ' .. nok .. should be ' .. cmd .. ' instead')
				end
			elseif cmd:sub(1, 2) == reply:sub(1, 2) then
				print(reply .. ' .. ok')
				return reply
			else
				print(reply .. ' .. nok')
			end	
		end
	end

	print(MAX_TRIES .. ' write attempts failed. Exiting ...')
	exit(3) 
end

--- Check the sensor board hardware version.
-- @param ctrl  	ctrl object
-- @return		none 
local function check_hw_version(ctrl) 
	local hw_major, hw_minor = send(ctrl, GET_HW_VERSION):match(GET_HW_VERSION_R)

	if hw_major ~= flukso.main.hw_major or hw_minor > flukso.main.hw_minor then
		print(string.format('Hardware check (major: %s, minor: %s) .. nok', hw_major, hw_minor))
		if hw_major ~= flukso.main.hw_major then
			print('Error. Major version does not match.')
		end

		if hw_minor > flukso.main.hw_minor then
			print('Error. Sensor board minor version is not supported by this package.')
		end

		if HW_CHECK_OVERRIDE then
			print('Overridden. Good luck!')
		else
			print('Use -f to override this check at your own peril.')
			exit(4)
		end
	else
		print(string.format('Hardware check (major: %s, minor: %s) .. ok', hw_major, hw_minor))
	end
end

--- Disable all sensors in the sensor board.
-- @param ctrl  	ctrl object
-- @return		none 
local function disable_all_sensors(ctrl)
	for i = 1, MAX_SENSORS do
		local cmd = string.format(SET_ENABLE, toc(i), 0)
		send(ctrl, cmd)
	end
end

--- Set all configurable hardware lines.
-- @param ctrl  	ctrl object
-- @return		none 
local function set_hardware_lines(ctrl)
	local cmd = string.format(SET_HW_LINES, ANALOG_ENABLE, UART_RX_INVERT, UART_TX_INVERT)
	send(ctrl, cmd)
end

--- Populate the physical (port) to logical (sensor) map on the sensor board.
-- @param ctrl  	ctrl object
-- @return		none 
local function set_phy_to_log(ctrl)
	local phy_to_log = {}

	for i = 1, MAX_SENSORS do
		if flukso[tostring(i)] ~= nil then
			if flukso[tostring(i)]['class'] == 'analog' and i > MAX_ANALOG_SENSORS then
				print(string.format('Error. Analog sensor %s should be less than or equal to max_analog_sensors (%s)', i, MAX_ANALOG_SENSORS))
				exit(5)
			end

			local ports = flukso[tostring(i)].port or {}

			for j = 1, #ports do
				if tonumber(ports[j]) > MAX_SENSORS then
					print(string.format('Error. Port numbering in sensor %s should be less than or equal to max_sensors (%s)', i, MAX_SENSORS))
					exit(6)

				else
					phy_to_log[toc(tonumber(ports[j]))] = toc(i)
				end
			end
		end
	end

	-- ports that are not in use are mapped to sensor id 0xff
	for i = 0, MAX_SENSORS - 1 do
		if not phy_to_log[i] then
			phy_to_log[i] = 0xff
		end
	end

	local cmd = SET_PHY_TO_LOG .. ' ' .. table.concat(phy_to_log, ' ', 0)
	send(ctrl, cmd)
end

--- Populate each sensor's meterconstant on the sensor board.
-- @param ctrl  	ctrl object
-- @return		none 
local function set_meterconst(ctrl)
	for i = 1, MAX_SENSORS do
		local cmd = { }

		if flukso[tostring(i)] == nil then
			cmd[1] = string.format(SET_METERCONST, toc(i), 0)

		elseif flukso[tostring(i)]['class'] == 'analog' then
			local voltage = tonumber(flukso[tostring(i)].voltage or "0")
			local current = tonumber(flukso[tostring(i)].current or "0")

			cmd[1] = string.format(SET_METERCONST, toc(i), math.floor(METERCONST_FACTOR * voltage * current))

		elseif flukso[tostring(i)]['class'] == 'pulse'then
			local real = tonumber(flukso[tostring(i)].constant or "0")
			local meterconst = math.floor(real)
			local fraction = math.floor((real % 1) * 1000)

			cmd[1] = string.format(SET_METERCONST, toc(i), meterconst)
			cmd[2] = string.format(SET_FRACTION, toc(i), fraction) 
		else
			cmd[1] = string.format(SET_METERCONST, toc(i), 0)
		end

		if not cmd[2] then
			cmd[2] = string.format(SET_FRACTION, toc(i), 0)
		end

		send(ctrl, cmd[1])
		send(ctrl, cmd[2])
	end
end

--- Reset each sensor's counter on the sensor board.
-- @param ctrl  	ctrl object
-- @return		none 
local function reset_counters(ctrl)
	for i = 1, MAX_SENSORS do
		local cmd = string.format(SET_COUNTER, toc(i), 0)
		send(ctrl, cmd)
	end

	uci:set('flukso', 'main', 'reset_counters', 0)
	uci:commit('flukso')
end

--- Activate the enabled sensors on the sensor board.
-- @param ctrl  	ctrl object
-- @return		none 
local function enable_sensors(ctrl)
	for i = 1, MAX_SENSORS do
		if flukso[tostring(i)] ~= nil and flukso[tostring(i)].enable == '1' then
			cmd = string.format(SET_ENABLE, toc(i), 1)
			send(ctrl, cmd)
		end
	end
end

--- Commit all changes on the sensor board.
-- @param ctrl  	ctrl object
-- @return		none 
local function commit(ctrl)
	send(ctrl, COMMIT)
end

--- Remove all /sensor/xyz endpoint mappings to the cgi script.
-- @return		none 
local function remove_symlinks()
	if nixio.fs.dir(API_PATH) then
		for symlink in nixio.fs.dir(API_PATH) do
			nixio.fs.unlink(API_PATH .. symlink)
		end
	end
end

--- Map /sensor/xyz endpoints to the cgi script.
-- @return		none 
local function create_symlinks()
	-- make sure /www/sensor exists
	nixio.fs.mkdirr(API_PATH)

	-- generate new symlinks
	for i = 1, LAST_PROV_SENSOR do
		if flukso[tostring(i)] ~= nil
			and flukso[tostring(i)].enable == '1'
			and flukso[tostring(i)].id
			and flukso[tostring(i)].class ~= 'uart'
			then

			local sensor_id = flukso[tostring(i)].id

			if sensor_id then
				nixio.fs.symlink(CGI_SCRIPT, API_PATH .. sensor_id)
				print(string.format('ln -s %s %s%s .. ok', CGI_SCRIPT, API_PATH, sensor_id))
			end
		end
	end
end

--- Remove the avahi-daemon flukso.service xml file.
-- @return		none 
local function remove_avahi_config()
	nixio.fs.unlink(AVAHI_PATH)
end

--- Generate a new flukso.service xml file for the avahi-daemon.
-- @return		none 
local function create_avahi_config()
	avahi = { head = {}, body = {}, tail = {} }

	avahi.head[1] = '<?xml version="1.0" standalone="no"?><!--*-nxml-*-->'
	avahi.head[2] = '<!DOCTYPE service-group SYSTEM "avahi-service.dtd">'
	avahi.head[3] = '<service-group>'
	avahi.head[4] = ' <name replace-wildcards="yes">Flukso RESTful API on %h</name>'
	avahi.head[5] = '  <service>'
	avahi.head[6] = '    <type>_flukso._tcp</type>'
	avahi.head[7] = '    <port>8080</port>'

	for i = 1, LAST_PROV_SENSOR do
		if flukso[tostring(i)] ~= nil
			and flukso[tostring(i)].enable == '1'
			and flukso[tostring(i)].id
			and flukso[tostring(i)].class ~= 'uart'
			then

			avahi.body[#avahi.body + 1] = string.format('    <txt-record>id%d=%s</txt-record>' , i, flukso[tostring(i)].id)
		end
	end

	avahi.tail[1] = '    <txt-record>path=/sensor</txt-record>'
	avahi.tail[2] = '    <txt-record>version=1.0</txt-record>'
	avahi.tail[3] = '  </service>'
	avahi.tail[4] = '</service-group>'

	-- generate the new flukso service
	fd = nixio.open(AVAHI_PATH, O_RDWR_CREAT)
	print(string.format('generating a new %s', AVAHI_PATH))

	for i = 1, #avahi.head do
		fd:write(avahi.head[i] .. '\n')
	end

	for i = 1, #avahi.body do
		fd:write(avahi.body[i] .. '\n')
	end

	for i = 1, #avahi.tail do
		fd:write(avahi.tail[i] .. '\n')
	end
end

--- POST each sensor's parameters to the /sensor/xyz endpoint
-- @return		none
local function phone_home()
	local function json_config(i) -- type(i) --> "string"
		local config = {}

		config["class"]    = flukso[i]["class"]
		config["type"]     = flukso[i]["type"]
		config["function"] = flukso[i]["function"]
		config["voltage"]  = tonumber(flukso[i]["voltage"])
		config["current"]  = tonumber(flukso[i]["current"])
		config["constant"] = tonumber(flukso[i]["constant"])
		config["enable"]   = tonumber(flukso[i]["enable"])

		if config["class"] == "analog" then
			local phase = tonumber(flukso.main.phase)

			if phase == 1 or 
			   phase == 3 and i == "1" then
				config["phase"] = phase
			end
		end

		return luci.json.encode{ config = config }
	end

	local headers = {}
	headers['Content-Type'] = 'application/json'
	headers['X-Version'] = '1.0'
	headers['User-Agent'] = USER_AGENT

	local options = {}
	options.sndtimeo = 5
	options.rcvtimeo = 5

	options.tls_context_set_verify = 'peer'
	options.cacert = CACERT
	options.method  = 'POST'
	options.headers = headers

	local http_persist = httpclient.create_persistent()

	local err = false

	for i = 1, LAST_PROV_SENSOR do
		if flukso[tostring(i)] ~= nil and flukso[tostring(i)].id then
			local sensor_id = flukso[tostring(i)].id

			if i ~= LAST_PROV_SENSOR then
				options.headers['Connection'] = 'keep-alive'
			else
				options.headers['Connection'] = 'close'
			end

			options.body = json_config(tostring(i))
			options.headers['Content-Length'] = tostring(#options.body)

			local hash = nixio.crypto.hmac('sha1', WAN_KEY)
			hash:update(options.body)
			options.headers['X-Digest'] = hash:final()

			local url = WAN_BASE_URL .. sensor_id
			local response, code, call_info = http_persist(url, options)

			local level

			if code == 200 or code == 204 then
				level = 'info'
			else
				level = 'err'
				err = true
			end

			nixio.syslog(level, string.format('%s %s: %s', options.method, url, code))

			-- if available, send additional error info to the syslog
			if type(call_info) == 'string' then
				nixio.syslog('err', call_info)
			elseif type(call_info) == 'table'  then
				local auth_error = call_info.headers['WWW-Authenticate']

				if auth_error then
					nixio.syslog('err', string.format('WWW-Authenticate: %s', auth_error))
				end
			end
		end
	end

	if err then
		exit(7)
	end
end

-- open the connection to the syslog deamon, specifying our identity
nixio.openlog('fsync', 'pid')

-- sync config to sensor board
local ctrl = ctrl_init()

check_hw_version(ctrl)
disable_all_sensors(ctrl)

if MODEL == 'FLM02B' or MODEL == 'FLM02C' then
	set_hardware_lines(ctrl)
end

set_phy_to_log(ctrl)
set_meterconst(ctrl)

if RESET_COUNTERS then
	reset_counters(ctrl)
end

enable_sensors(ctrl)
commit(ctrl)

ctrl_close(ctrl)


-- sync config locally
remove_symlinks()
remove_avahi_config()

if LAN_ENABLED then
	create_symlinks()
	create_avahi_config()
end

-- sync config with the server
if WAN_ENABLED then
	phone_home()
end


print(arg[0] .. ' completed successfully. Bye!')
exit(0)
