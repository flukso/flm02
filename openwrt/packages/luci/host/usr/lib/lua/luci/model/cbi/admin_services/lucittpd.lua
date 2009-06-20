--[[
LuCI - Lua Configuration Interface

Copyright 2008 Steven Barth <steven@midlink.org>
Copyright 2008 Jo-Philipp Wich <xm@leipzig.freifunk.net>

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

	http://www.apache.org/licenses/LICENSE-2.0

$Id: lucittpd.lua 3941 2008-12-23 21:39:38Z jow $
]]--
m = Map("lucittpd", "LuCIttpd", translate("a_srv_lucittpd"))

s = m:section(NamedSection, "lucittpd", "lucittpd", "")

s:option(Value, "port", translate("port"))
s:option(Value, "root", translate("a_srv_http_root"))
s:option(Value, "path", translate("a_srv_http_path"))
s:option(Flag, "keepalive", translate("a_srv_http_keepalive"))
s:option(Value, "timeout", translate("a_srv_http_timeout"))

return m
