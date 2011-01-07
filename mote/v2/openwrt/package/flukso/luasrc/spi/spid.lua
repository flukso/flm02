#! /usr/bin/env lua

--[[
    
    spid.lua - Lua part of the spi daemon

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


require 'nixio.fs'

ctrl_path = os.getenv('DAEMON_PATH') .. '/ctrl/'  

nixio.fs.mkdirr(ctrl_path)
nixio.fs.unlink(ctrl_path .. 'in')
nixio.fs.unlink(ctrl_path .. 'out')

nixio.fs.mkfifo(ctrl_path .. 'in', '644')
nixio.fs.mkfifo(ctrl_path .. 'out', '644')

rdwr_nonblock = nixio.open_flags('rdwr', 'nonblock')

ctrl_fd_in = nixio.open(ctrl_path .. 'in', rdwr_nonblock)
ctrl_fd_out = nixio.open(ctrl_path .. 'out', rdwr_nonblock)

pfin = nixio.poll_flags('in')

local fd = {fd      = ctrl_fd_in,
            events  = pfin,
            revents = 0}
            
local fds = {fd}
                        
ctrl_line = ctrl_fd_in:linesource()

while true do
	if nixio.poll(fds, 1000) then
		ctrl_fd_out:write((ctrl_line() or 'nil!') .. '\n')
	else
		ctrl_fd_out:write('timeout\n')	
	end	
end

nixio.fs.unlink(ctrl_path .. 'in') 
nixio.fs.unlink(ctrl_path .. 'out')
