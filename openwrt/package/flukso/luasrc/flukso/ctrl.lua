--[[
    
    ctrl.lua - Lua 5.1 flukso module for sending commands to the sensor board

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
require 'nixio.util'

local string = string

local getfenv, setmetatable =
      getfenv, setmetatable


module (...)
local modenv = getfenv()

local CTRL_PATH		 = '/var/run/spid/ctrl'
local CTRL_PATH_IN	 = CTRL_PATH .. '/in'
local CTRL_PATH_OUT	 = CTRL_PATH .. '/out'

local O_RDWR_NONBLOCK	 = nixio.open_flags('rdwr', 'nonblock')
local O_RDWR_CREAT	 = nixio.open_flags('rdwr', 'creat')
local POLLIN		 = nixio.poll_flags('in')
local POLL_TIMEOUT_MS	 = 1000
local MAX_TRIES		 = 5

--- Initialise communication to the ctrl fifos of the spid
-- @return		ctrl object containing the fd's, a line-based iterator and poll flags 
function init()
	local ctrl = { fdin    = nixio.open(CTRL_PATH_IN, O_RDWR_NONBLOCK),
                       fdout   = nixio.open(CTRL_PATH_OUT, O_RDWR_NONBLOCK),
                       events  = POLLIN,
                       revents = 0 }

	if ctrl.fdin == nil or ctrl.fdout == nil then
		return nil, 'unable to open ctrl fifos'
	end

	-- block until we can acquire a lock on both ctrl fifos
	ctrl.fdin:lock('lock')
	ctrl.fdout:lock('lock')

	ctrl.fd = ctrl.fdout -- need this entry for nixio.poll
	ctrl.line = ctrl.fdout:linesource()

	return setmetatable(ctrl, { __index = modenv })
end

--- Close the spid control fifos.
-- @param code  	ctrl object
-- @return		none 
function close(ctrl)
	ctrl.fdin:close()
	ctrl.fdout:close()
end

--- Send a command down the control fifo.
-- @param ctrl  	ctrl object
-- @param cmd		command to send
-- @return		false | true | reply args
function send(ctrl, cmd)
	-- flush the ctrl out fifo
	while ctrl.line() do end

	for i = 1, MAX_TRIES do
		ctrl.fdin:write(cmd .. '\n')

		local poll, errno, errmsg = nixio.poll({ ctrl }, POLL_TIMEOUT_MS)

		if poll < 0 then
			nixio.syslog('err', string.format('Poll failed with error message: %s', errmsg))

		elseif poll == 0 then
			nixio.syslog('err', string.format('Poll timed out after: %d ms', POLL_TIMEOUT_MS))

		elseif poll > 0 then
			reply = ctrl.line()

			if cmd:sub(1, 1) == 's' or cmd == 'ct' then
				if reply == cmd then
					return true
				else
					return false
				end
			elseif cmd:sub(1, 2) == reply:sub(1, 2) then
				return reply
			else
				-- we received a reply not related to our cmd
				return false
			end
		end
	end

	nixio.syslog('err', string.format('%d write attempts failed', MAX_TRIES))
	return false
end
