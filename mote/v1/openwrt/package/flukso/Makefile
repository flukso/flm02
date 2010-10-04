# Copyright (c) 2008 jokamajo.org
#               2010 flukso.net
# $Id$

include $(TOPDIR)/rules.mk

PKG_NAME:=flukso
PKG_VERSION:=1.1
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
	$(CP) $(PKG_BUILD_DIR)/{flukso,data,dbg,auth,heartbeat,restful}.lua $(1)/usr/share/lua/flukso/
	$(INSTALL_DIR) $(1)/etc/init.d/
	$(CP) $(PKG_BUILD_DIR)/flukso.init $(1)/etc/init.d/flukso
	$(INSTALL_DIR) $(1)/etc/config/
	$(CP) $(PKG_BUILD_DIR)/flukso.uci $(1)/etc/config/flukso
endef

$(eval $(call BuildPackage,flukso))
