#!/usr/bin/env lua

if not arg[2] then
	print(string.format("Usage %s <hw_type> <sw_version>", arg[0]))
	os.exit(1)
end

local nixio = require "nixio"
require "nixio.util"
local uci = require "luci.model.uci".cursor()

local MOD = 64 --firmware length should be a 64byte multiple
local PAYLOAD_LENGTH = 64
local FF = string.char(255)
local KUBE = uci:get_all("kube")
local O_RDWR_CREAT = nixio.open_flags("rdwr", "creat")
local O_RDWR_APPEND = nixio.open_flags("rdwr", "append")

local hw_type = arg[1]
local sw_version = arg[2]
local path = string.format("%s/firmware/%d.%d", KUBE.main.cache, hw_type, sw_version)
local fd = assert(nixio.open(path, O_RDWR_APPEND), "Firmware not found. Exiting...")
local stat = fd:stat()
local padding = MOD - stat.size % MOD

if (padding < MOD) then
	fd:write(string.rep(FF, padding))
	fd:sync(true)
	fd:seek(0, "set")
end

local firmware = fd:readall()
local crc16 = nixio.bin.crc16(firmware)
print(string.format("Firmware %d.%d has crc16 of %d", hw_type, sw_version, crc16))
fd:close()

path = string.format("%s/firmware/%d.%d.white", KUBE.main.cache, hw_type, sw_version)
local fd = assert(nixio.open(path, O_RDWR_CREAT))
local firmware_white = nixio.bin.white(firmware)
fd:write(firmware_white)
fd:sync(true)
fd:close()

