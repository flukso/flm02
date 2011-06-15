--[[
    
    rrd.pipe - Lua 5.1 submodule for connecting to rrdtool in pipe mode

    Copyright (C) 2010 Bart Van Der Meerssche <bart.vandermeerssche@flukso.net>

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

require 'nixio'

local nixio, os =
      nixio, os

local getfenv, setmetatable =
      getfenv, setmetatable

module (...)
local modenv = getfenv()

--- Start an rrdtool server in pipe mode. Results in an rrdtool coprocess.
--
--   +----------+                  +-----------+
--   |          | fd[4] ---> fd[3] |           |
--   |  parent  |                  |  rrdtool  |
--   |          | fd[5] <--- fd[6] |           |
--   +----------+                  +-----------+
--
-- @return rrd.server (RS) object
function start()
	local fd = {}
	fd[3], fd[4] = nixio.pipe()
	fd[5], fd[6] = nixio.pipe()

	local pid, code, err = nixio.fork()

	if pid and pid ~= 0 then  -- parent
		fd[3]:close()
		fd[6]:close()
		fd[5]:setblocking(false)
		return setmetatable({pid = pid, fdr = fd[5], fdw = fd[4]}, {__index = modenv})
	elseif pid == 0 then      -- child
		nixio.dup(fd[3], nixio.stdin)
		nixio.dup(fd[6], nixio.stdout)
		for i = 3,6 do
			fd[i]:close()
		end 
		nixio.execp('rrdtool', '-')
		nixio.syslog('err', 'Unable to launch rrdtool')
		-- Send SIGTERM to parent process, quite drastic I know.
		nixio.kill(pid, nixio.const.SIGTERM)
	else                      -- error
		nixio.syslog('err', 'Unable to fork(): ' .. err)
		return nil, code, err
	end
end

--- Stop the rrdtool server.
-- @param   rrd.server (RS) object
-- @return  waitpid return values
-- @see nixio.waitpid
function stop(RS)
	RS.fdr:close()
	RS.fdw:close()
	nixio.kill(RS.pid, nixio.const.SIGTERM)
	return nixio.waitpid(RS.pid)
end
