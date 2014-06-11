#
# Copyright (C) 2014 OpenWrt.org
#
# This is free software, licensed under the GNU General Public License v2.
# See /LICENSE for more information.
#

include $(TOPDIR)/rules.mk

PKG_NAME:=gzio
PKG_VERSION:=1.0.1
PKG_RELEASE:=1

PKG_SOURCE_PROTO:=git
PKG_SOURCE_URL:=git://github.com/flukso/$(PKG_NAME).git
PKG_SOURCE_SUBDIR:=$(PKG_NAME)-$(PKG_VERSION)
PKG_SOURCE_VERSION:=1ee8885741736a1641a1e06c0eb15cc8f1ef99f7
PKG_SOURCE:=$(PKG_NAME)-$(PKG_VERSION)-$(PKG_SOURCE_VERSION).tar.bz2

include $(INCLUDE_DIR)/package.mk

define Package/gzio
    SUBMENU:=Lua
    SECTION:=lang
    CATEGORY:=Languages
    TITLE:=gzio
    DEPENDS:=+lua +zlib
    MAINTAINER:=Bart Van Der Meerssche <bart@flukso.net>
endef

define Package/gzio/description
	The Lua gzip file I/O module emulates the standard I/O module,
	but operates on compressed gzip format files.
endef

define Package/gzio/install
	$(INSTALL_DIR) $(1)/usr/lib/lua
	$(INSTALL_BIN) $(PKG_BUILD_DIR)/gzio.so $(1)/usr/lib/lua
endef

$(eval $(call BuildPackage,gzio))
