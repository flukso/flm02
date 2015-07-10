#
# Copyright (C) 2014 OpenWrt.org
#
# This is free software, licensed under the GNU General Public License v2.
# See /LICENSE for more information.
#

include $(TOPDIR)/rules.mk

PKG_NAME:=vstruct
PKG_VERSION:=2013-11-04
PKG_RELEASE=$(PKG_SOURCE_VERSION)

PKG_SOURCE_PROTO:=git
PKG_SOURCE_URL:=git://github.com/ToxicFrog/$(PKG_NAME).git
PKG_SOURCE_SUBDIR:=$(PKG_NAME)-$(PKG_VERSION)
PKG_SOURCE_VERSION:=e3de5dc1d3f64810805d024ea87222a7759980ee
PKG_SOURCE:=$(PKG_NAME)-$(PKG_VERSION)-$(PKG_SOURCE_VERSION).tar.gz

PKG_MAINTAINER:=Bart Van Der Meerssche <bart@flukso.net>

include $(INCLUDE_DIR)/package.mk

define Package/vstruct
    SUBMENU:=Lua
    SECTION:=lang
    CATEGORY:=Languages
    TITLE:=Vstruct
    DEPENDS:=+lua
endef

define Package/vstruct/description
	A Lua library for packing and unpacking binary data, supporting arbitrary (byte-aligned) widths, named fields, and repetition.
endef

define Build/Compile
endef

define Package/vstruct/install
	$(INSTALL_DIR) $(1)/usr/lib/lua
	$(CP) $(PKG_BUILD_DIR)/vstruct $(1)/usr/lib/lua
endef

$(eval $(call BuildPackage,vstruct))
