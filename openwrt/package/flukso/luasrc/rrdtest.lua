#! /usr/bin/env lua

--[[
    
    test - rrd test code

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

require 'nixio.util'
require 'rrd.server'

local rrdserv = rrd.server:start()
local lines = rrdserv.fdr:linesource()

rrdserv.fdw:write('info\n')

for line, code, err in lines do
	print(line)
end
