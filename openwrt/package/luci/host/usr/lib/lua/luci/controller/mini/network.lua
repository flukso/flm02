--[[
LuCI - Lua Configuration Interface

Copyright 2008 Steven Barth <steven@midlink.org>
Copyright 2008 Jo-Philipp Wich <xm@leipzig.freifunk.net>

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

	http://www.apache.org/licenses/LICENSE-2.0

$Id: network.lua 3674 2008-10-31 09:35:53Z Cyrus $
]]--

module("luci.controller.mini.network", package.seeall)

function index()
	luci.i18n.loadc("admin-core")
	local i18n = luci.i18n.translate

	-- entry({"network"}, alias("mini", "network", "index"), i18n("network"), 20).index = true
	entry({"network"}, cbi("mini/network", {autoapply=true}), i18n("network"), 1)
	entry({"wifi"}, cbi("mini/wifi", {autoapply=true}), i18n("wifi"), 10).i18n="wifi"
	-- entry({"network", "dhcp"}, cbi("mini/dhcp", {autoapply=true}), "DHCP", 20)
end
