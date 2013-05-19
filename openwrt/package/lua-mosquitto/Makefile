#
# Copyright (C) 2013 OpenWrt.org
#
# This is free software, licensed under the GNU General Public License v2.
# See /LICENSE for more information.
#

include $(TOPDIR)/rules.mk

PKG_NAME:=lua-mosquitto
PKG_VERSION:=0.1
PKG_RELEASE:=1

PKG_SOURCE:=$(PKG_NAME)-$(PKG_VERSION).tar.bz2
PKG_SOURCE_SUBDIR:=$(PKG_NAME)-$(PKG_VERSION)
PKG_SOURCE_URL:=git://github.com/flukso/$(PKG_NAME).git
PKG_SOURCE_PROTO:=git
PKG_SOURCE_VERSION:=28add65e9d12a7adbedaf483489544923a870462

include $(INCLUDE_DIR)/package.mk

define Package/lua-mosquitto
    SUBMENU:=Lua
    SECTION:=lang
    CATEGORY:=Languages
    TITLE:=Lua-mosquitto
    DEPENDS:=+libmosquitto +lua
    MAINTAINER:=Bart Van Der Meerssche <bart@flukso.net>
endef

define Package/lua-mosquitto/description
	Lua bindings to libmosquitto
endef

define Package/lua-mosquitto/install
	$(INSTALL_DIR) $(1)/usr/lib/lua
	$(INSTALL_BIN) $(PKG_BUILD_DIR)/mosquitto.so $(1)/usr/lib/lua
	$(INSTALL_DIR) $(1)/usr/share/ring
	$(INSTALL_BIN) $(PKG_BUILD_DIR)/test/ring/* $(1)/usr/share/ring/
endef

$(eval $(call BuildPackage,lua-mosquitto))
