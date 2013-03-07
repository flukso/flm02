#
# Copyright (C) 2012 OpenWrt.org
#
# This is free software, licensed under the GNU General Public License v2.
# See /LICENSE for more information.
#

include $(TOPDIR)/rules.mk

PKG_NAME:=luazmq
PKG_VERSION:=1.1
PKG_RELEASE:=1

PKG_SOURCE:=$(PKG_NAME)-$(PKG_VERSION).tar.bz2
PKG_SOURCE_SUBDIR:=$(PKG_NAME)-$(PKG_VERSION)
PKG_SOURCE_URL:=git://github.com/Neopallium/lua-zmq.git
PKG_SOURCE_PROTO:=git
PKG_SOURCE_VERSION:=78ac082f628971bf69b04289a735a958286e06ec

PKG_INSTALL:=1
CMAKE_INSTALL:=1

include $(INCLUDE_DIR)/package.mk
include $(INCLUDE_DIR)/cmake.mk

define Package/luazmq
    SUBMENU:=Lua
    SECTION:=lang
    CATEGORY:=Languages
    TITLE:=Lua-zmq
    DEPENDS:=+libzmq +lua
endef

define Package/luazmq/description
	Lua bindings for libzmq
endef

define Package/luazmq/install
	$(INSTALL_DIR) $(1)/usr/lib/lua
	$(INSTALL_BIN) $(PKG_BUILD_DIR)/zmq.so $(1)/usr/lib/lua
	$(INSTALL_DIR) $(1)/usr/lib/lua/zmq
	$(INSTALL_DATA) $(PKG_BUILD_DIR)/src/{poller,threads}.lua $(1)/usr/lib/lua/zmq
endef

$(eval $(call BuildPackage,luazmq))
