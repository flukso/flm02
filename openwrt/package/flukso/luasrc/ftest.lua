#!/usr/bin/env lua

--[[
    
    ftest.lua - check sensor board operation

    Copyright (C) 2012 Bart Van Der Meerssche <bart.vandermeerssche@flukso.net>

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

local uci    = require 'luci.model.uci'.cursor()
local ctrl   = require 'flukso.ctrl'.init()

-- parse and load /etc/config/flukso
local FLUKSO = uci:get_all('flukso')

-- sensor board commands
local GET_HW_VERSION    = 'gh'
local GET_HW_VERSION_R  = '^gh%s+(%d+)%s+(%d+)$'

nixio.openlog('ftest', 'pid')


local reply = ctrl:send(GET_HW_VERSION)

if reply then
	local hw_major, hw_minor = reply:match(GET_HW_VERSION_R)

	if hw_major ~= FLUKSO.main.hw_major or hw_minor ~= FLUKSO.main.hw_minor then
		nixio.syslog('err', 'non-matching sensor board hardware versions')
		os.exit(2)
	end
else
	nixio.syslog('err', 'communication with sensor board failed')
	os.exit(1)
end
