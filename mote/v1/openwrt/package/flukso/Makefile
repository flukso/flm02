# Copyright (c) 2008 jokamajo.org
# $Id$

include $(TOPDIR)/rules.mk

PKG_NAME:=flukso
PKG_VERSION:=1.0
PKG_RELEASE:=1

PKG_BUILD_DIR:=$(BUILD_DIR)/$(PKG_NAME)-$(PKG_VERSION)

include $(INCLUDE_DIR)/package.mk

define Package/flukso
  SECTION:=utils
  CATEGORY:=Utilities
  DEPENDS:=+liblua-xmlrpc +ntpclient
  TITLE:=Flukso - community metering
endef

define Package/flukso/description
	This package contains the necessary openwrt files for the Flukso [www.flukso.net] community metering application.
endef

define Build/Prepare
	mkdir -p $(PKG_BUILD_DIR)
	$(CP) ./src/* $(PKG_BUILD_DIR)/
endef

define Build/Compile
endef

define Package/flukso/install
	$(INSTALL_DIR) $(1)/usr/share/lua/flukso/
	$(CP) $(PKG_BUILD_DIR)/flukso.lua $(1)/usr/share/lua/flukso/
	$(CP) $(PKG_BUILD_DIR)/data.lua $(1)/usr/share/lua/flukso/
	$(CP) $(PKG_BUILD_DIR)/dbg.lua $(1)/usr/share/lua/flukso/
	$(CP) $(PKG_BUILD_DIR)/auth.lua $(1)/usr/share/lua/flukso/
	$(CP) $(PKG_BUILD_DIR)/heartbeat.lua $(1)/usr/share/lua/flukso/
	$(INSTALL_DIR) $(1)/etc/init.d/
	$(CP) $(PKG_BUILD_DIR)/flukso.init $(1)/etc/init.d/flukso
endef

$(eval $(call BuildPackage,flukso))
