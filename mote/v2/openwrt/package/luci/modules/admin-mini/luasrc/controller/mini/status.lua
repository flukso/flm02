--[[
LuCI - Lua Configuration Interface

Copyright 2008 Steven Barth <steven@midlink.org>

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

	http://www.apache.org/licenses/LICENSE-2.0

$Id: status.lua 5118 2009-07-23 03:32:30Z jow $
]]--
module("luci.controller.mini.status", package.seeall)

function index()
	luci.i18n.loadc("admin-core")
	local i18n = luci.i18n.translate

	entry({"syslog"}, call("action_syslog"), i18n("syslog", "System Log"), 5)
	entry({"dmesg"}, call("action_dmesg"), i18n("dmesg", "Kernel Log"), 6)

end

function action_syslog()
	local syslog = luci.sys.syslog()
	luci.template.render("mini/syslog", {syslog=syslog})
end

function action_dmesg()
	local dmesg = luci.sys.dmesg()
	luci.template.render("mini/dmesg", {dmesg=dmesg})
end
