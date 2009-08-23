--[[
LuCI - Lua Configuration Interface

Copyright 2008 Steven Barth <steven@midlink.org>
Copyright 2008 Jo-Philipp Wich <xm@leipzig.freifunk.net>

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

	http://www.apache.org/licenses/LICENSE-2.0

$Id: wifi.lua 4223 2009-02-04 19:20:17Z Cyrus $
]]--

-- Data init --

local uci = luci.model.uci.cursor()
if not uci:get("network", "wan") then
	uci:section("network", "interface", "wan", {proto="none", ifname=" "})
	uci:save("network")
	uci:commit("network")
end

local wlcursor = luci.model.uci.cursor_state()
local wireless = wlcursor:get_all("wireless")
local wifidata = luci.sys.wifi.getiwconfig()
local wifidevs = {}
local ifaces = {}

for k, v in pairs(wireless) do
	if v[".type"] == "wifi-iface" then
		table.insert(ifaces, v)
	end
end

wlcursor:foreach("wireless", "wifi-device",
	function(section)
		table.insert(wifidevs, section[".name"])
	end)


-- Main Map --

m = Map("wireless", translate("wifi"), translate("a_w_devices1"))
m:chain("network")

if #wifidevs < 1 then
	return m
end

-- Config Section --

local hwtype = m:get(wifidevs[1], "type")


s = m:section(TypedSection, "wifi-iface", "")
s.anonymous = true
s.addremove = false

s:option(Value, "ssid", translate("a_w_netid"))

bssid = s:option(Value, "bssid", translate("wifi_bssid"))

local devs = {}
luci.model.uci.cursor():foreach("wireless", "wifi-device",
	function (section)
		table.insert(devs, section[".name"])
	end)
	
if #devs > 1 then
	device = s:option(DummyValue, "device", translate("device"))
else
	s.defaults.device = devs[1]
end

encr = s:option(ListValue, "encryption", translate("encryption"))
encr.override_values = true
encr:value("none", "No Encryption")
encr:value("wep", "WEP")

if hwtype == "atheros" or hwtype == "mac80211" then
	local supplicant = luci.fs.mtime("/usr/sbin/wpa_supplicant")
	local hostapd = luci.fs.mtime("/usr/sbin/hostapd")

	if hostapd and supplicant then
		encr:value("psk", "WPA-PSK")
		encr:value("psk2", "WPA2-PSK")
		encr:value("wpa", "WPA-Radius", {mode="ap"})
		encr:value("wpa2i", "WPA2-Radius", {mode="ap"})
	elseif hostapd and not supplicant then
		encr:value("psk", "WPA-PSK", {mode="ap"}, {mode="adhoc"})
		encr:value("psk2", "WPA2-PSK", {mode="ap"}, {mode="adhoc"})
		encr:value("wpa", "WPA-Radius", {mode="ap"})
		encr:value("wpa2i", "WPA2-Radius", {mode="ap"})
		encr.description = translate("wifi_wpareq")
	elseif not hostapd and supplicant then
		encr:value("psk", "WPA-PSK", {mode="sta"})
		encr:value("psk2", "WPA2-PSK", {mode="sta"})
		encr.description = translate("wifi_wpareq")
	else
		encr.description = translate("wifi_wpareq")
	end
elseif hwtype == "broadcom" then
	encr:value("psk", "WPA-PSK")
	encr:value("psk2", "WPA2-PSK")
end

key = s:option(Value, "key", translate("key"))
key:depends("encryption", "wep")
key:depends("encryption", "psk")
key:depends("encryption", "wpa")
key:depends("encryption", "psk2")
key:depends("encryption", "wpa2i")
key.rmempty = true
key.description = translate('wifi_keyreq')

function key:validate(value, section)
        function string.tohex(x)
                local hex = ''
                for c in x:gmatch('(.)') do hex = hex .. string.format("%02x", c:byte()) end
                return hex
        end

        function string.hexcheck(x)
                return #(x:match('%x*')) == #x and x
        end

        if encr:formvalue(section) == 'wep' then
                if #value == 5 or #value == 13 then
                        return value:tohex()
                elseif #value == 10 or #value == 26 then
                        return value:hexcheck()
                else
                        return nil
                end
        elseif encr:formvalue(section) == 'psk' or encr:formvalue(section) == 'psk2' then
                return #value > 7 and #value < 64 and value
        else
                return value
        end
end

server = s:option(Value, "server", translate("a_w_radiussrv"))
server:depends("encryption", "wpa")
server:depends("encryption", "wpa2i")
server.rmempty = true

port = s:option(Value, "port", translate("a_w_radiusport"))
port:depends("encryption", "wpa")
port:depends("encryption", "wpa2i")
port.rmempty = true


if hwtype == "atheros" or hwtype == "broadcom" then
	iso = s:option(Flag, "isolate", translate("a_w_apisolation"), translate("a_w_apisolation1"))
	iso.rmempty = true
	iso:depends("mode", "ap")
	
	hide = s:option(Flag, "hidden", translate("a_w_hideessid"))
	hide.rmempty = true
	hide:depends("mode", "ap")
end

if hwtype == "mac80211" or hwtype == "atheros" then
	bssid:depends({mode="adhoc"})
end

if hwtype == "broadcom" then
	bssid:depends({mode="wds"})
end


return m
