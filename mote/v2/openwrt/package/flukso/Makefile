# Copyright (c) 2010 flukso.net
# $Id$

include $(TOPDIR)/rules.mk

PKG_NAME:=flukso
PKG_VERSION:=2.0
PKG_RELEASE:=1

PKG_BUILD_DIR:=$(BUILD_DIR)/$(PKG_NAME)-$(PKG_VERSION)

include $(INCLUDE_DIR)/package.mk

define Package/flukso
  SECTION:=utils
  CATEGORY:=Utilities
  DEPENDS:=+ntpclient +nixio +rrdtool1
  TITLE:=Flukso - community metering
endef

define Package/flukso/description
	This package contains the Flukso daemon code and support libraries.
endef

define Build/Prepare
	mkdir -p $(PKG_BUILD_DIR)
	$(CP) ./luasrc/* $(PKG_BUILD_DIR)/
endef


define Build/Compile
endef

define Package/flukso/install
	$(INSTALL_DIR) $(1)/usr/lib/lua
	$(CP) $(PKG_BUILD_DIR)/dbg.lua $(1)/usr/lib/lua/
	$(INSTALL_DIR) $(1)/usr/lib/lua/rrd
	$(CP) $(PKG_BUILD_DIR)/rrd/*.lua $(1)/usr/lib/lua/rrd/
endef

$(eval $(call BuildPackage,flukso))
