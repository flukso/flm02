#!/usr/bin/env lua

--[[
    
    ftic.lua - provision TIC (télé-info client) sensors in flukso uci

    Copyright (C) 2013 Bart Van Der Meerssche <bart@flukso.net>

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


local uci = require "luci.model.uci".cursor()

local MAX_SENSORS      = tonumber(uci:get("flukso", "main", "max_sensors"))
local MAX_PROV_SENSORS = tonumber(uci:get("flukso", "main", "max_provisioned_sensors"))

local function free_sensor()
	for i = MAX_SENSORS + 1, MAX_PROV_SENSORS do
		if not uci:get("flukso", tostring(i), "enable") then
			return i
		end
	end

	return false
end

local function p1_prov_electricity(i)
	local obis_list = {
		"BASE",
		"HCHC",
		"HCHP",
		"EJPHN",
		"EJPHPM",
		"BBRHCJB",
		"BBRHPJB",
		"BBRHCJW",
		"BBRHPJW",
		"BBEHCJR",
		"BBRHPJR",
		"PAPP"
	}

	uci:set("flukso", tostring(i), "type", "electricity")
	uci:set("flukso", tostring(i), "function", "smart-main")
	uci:set("flukso", tostring(i), "class", "cosem")
	uci:set_list("flukso", tostring(i), "obis", obis_list)
	uci:set("flukso", tostring(i), "port", "ttyS0")
	uci:set("flukso", tostring(i), "protocol", "tic")
	uci:set("flukso", tostring(i), "derive", "1")
	uci:set("flukso", tostring(i), "enable", "1")
end

p1_prov_electricity(free_sensor())
uci:commit("flukso")
