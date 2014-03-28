#!/usr/bin/env lua

if not arg[3] then
	print(string.format("Usage %s <binary> <hw_type> <sw_version>", arg[0]))
	os.exit(1)
end

local nixio = require "nixio"
nixio.util = require "nixio.util"
local uci = require "luci.model.uci".cursor()

local MOD = 64 --firmware length should be a 64byte multiple
local PAYLOAD_LENGTH = 64
local FF = string.char(255)
local KUBE = uci:get_all("kube")
local O_RDONLY = nixio.open_flags("rdonly")
local O_RDWR_CREAT = nixio.open_flags("rdwr", "creat")

local binary = arg[1]
local hw_type = arg[2]
local sw_version = arg[3]

local path = string.format("%s/firmware/%d.%d", KUBE.main.cache, hw_type, sw_version)
local fd = assert(nixio.open(binary, O_RDONLY), string.format("Binary %s not found.", binary))
local firmware = fd:readall()
local stat = fd:stat()
fd:close()

local padding = MOD - stat.size % MOD
if (padding < MOD) then
	firmware = firmware .. string.rep(FF, padding)
end

local crc16 = nixio.bin.crc16(firmware)
local size = firmware:len() / 16
local tpl = "Firmware %d.%d has size = %d and crc16 = %d."
print(string.format(tpl, hw_type, sw_version, size, crc16))
local fd = nixio.open(path, O_RDWR_CREAT)
local firmware_white = nixio.bin.white(firmware)
fd:write(firmware_white)
fd:sync(true)
fd:close()
