--[[
LuCI - Lua Configuration Interface

Copyright 2008 Steven Barth <steven@midlink.org>
Copyright 2008 Jo-Philipp Wich <xm@leipzig.freifunk.net>
Copyright 2011 Bart Van Der Meerssche <bart.vandermeerssche@flukso.net>

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

	http://www.apache.org/licenses/LICENSE-2.0

$Id: network.lua 3672 2008-10-31 09:35:11Z Cyrus $
]]--

module("luci.controller.mini.sensor", package.seeall)

function index()
	luci.i18n.loadc("admin-core")
	local i18n = luci.i18n.translate

	entry({"sensor"}, cbi("mini/sensor", {autoapply=true}), i18n("sensor"), 1)
end
