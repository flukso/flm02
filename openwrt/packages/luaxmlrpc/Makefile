# 
# Copyright (C) 2006 OpenWrt.org
#
# This is free software, licensed under the GNU General Public License v2.
# See /LICENSE for more information.
#
# $Id: Makefile 6994 2008-07-31 21:12:40Z icarus75 $

include $(TOPDIR)/rules.mk

PKG_NAME:=luaxmlrpc
PKG_VERSION:=1.0x
PKG_RELEASE:=1

PKG_BUILD_DIR:=$(BUILD_DIR)/$(PKG_NAME)-$(PKG_VERSION)

include $(INCLUDE_DIR)/package.mk

define Package/liblua-xmlrpc
  SECTION:=lang
  SUBMENU:=LUA
  CATEGORY:=Languages
  DEPENDS:=+liblua-expat +liblua-socket
  URL:=http://luaforge.net/projects/luaxmlrpc/
  TITLE:=XML-RPC library for LUA programming language
endef

define Package/liblua-xmlrpc/description
          LuaXMLRPC is a Lua library to make remote procedure calls using XML-RPC.
endef

define Build/Prepare
	mkdir -p $(PKG_BUILD_DIR)/src/
	$(CP) ./src/* $(PKG_BUILD_DIR)/src/
endef

define Build/Compile
endef

define Package/liblua-xmlrpc/install
	$(INSTALL_DIR) $(1)/usr/share/lua/xmlrpc/
	$(CP) $(PKG_BUILD_DIR)/src/*.lua $(1)/usr/share/lua/xmlrpc/
endef

$(eval $(call BuildPackage,liblua-xmlrpc))
