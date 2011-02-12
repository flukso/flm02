# Copyright (c) 2010-2011 Bart Van Der Meerssche

include $(TOPDIR)/rules.mk

PKG_NAME:=flukso
PKG_VERSION:=2.0-alpha
PKG_RELEASE:=1

PKG_BUILD_DIR:=$(BUILD_DIR)/$(PKG_NAME)-$(PKG_VERSION)

include $(INCLUDE_DIR)/package.mk

define Package/flukso
	SECTION:=utils
	CATEGORY:=Utilities
	DEPENDS:=+libdaemon +ntpclient +liblua +luci-nixio +rrdtool1
	TITLE:=Flukso - community metering
	URL:=http://www.flukso.net
	MAINTAINER:=Bart Van Der Meerssche <bart.vandermeerssche@flukso.net>
endef

define Package/flukso/description
	This package contains the Flukso daemon code and support libraries.
endef

define Build/Prepare
	mkdir -p $(PKG_BUILD_DIR)
	$(CP) ./src/* $(PKG_BUILD_DIR)/
	$(CP) ./luasrc/* $(PKG_BUILD_DIR)/
	$(CP) ./config/* $(PKG_BUILD_DIR)/
endef

define Package/flukso/install
	$(INSTALL_DIR) $(1)/etc/config
	$(INSTALL_DATA) $(PKG_BUILD_DIR)/flukso.uci $(1)/etc/config/flukso
	$(INSTALL_DIR) $(1)/etc/init.d
	$(INSTALL_BIN) $(PKG_BUILD_DIR)/flukso.init $(1)/etc/init.d/flukso
	$(INSTALL_DIR) $(1)/usr/lib/lua
	$(INSTALL_DATA) $(PKG_BUILD_DIR)/dbg.lua $(1)/usr/lib/lua/
	$(INSTALL_DIR) $(1)/usr/lib/lua/rrd
	$(INSTALL_DATA) $(PKG_BUILD_DIR)/rrd/*.lua $(1)/usr/lib/lua/rrd/
	$(INSTALL_DIR) $(1)/usr/lib/lua/flukso
	$(INSTALL_DATA) $(PKG_BUILD_DIR)/flukso/*.lua $(1)/usr/lib/lua/flukso/
	$(INSTALL_DIR) $(1)/usr/sbin
	$(INSTALL_BIN) $(PKG_BUILD_DIR)/luad $(1)/usr/sbin/
	$(LN) /usr/sbin/luad $(1)/usr/sbin/fluksod
	$(INSTALL_BIN) $(PKG_BUILD_DIR)/fluksod.lua $(1)/usr/sbin/
	$(LN) /usr/sbin/luad $(1)/usr/sbin/spid
	$(INSTALL_BIN) $(PKG_BUILD_DIR)/spid.lua $(1)/usr/sbin/
	$(INSTALL_DIR) $(1)/usr/bin
	$(INSTALL_BIN) $(PKG_BUILD_DIR)/fsync.lua $(1)/usr/bin/fsync
	$(INSTALL_BIN) $(PKG_BUILD_DIR)/fdude.lua $(1)/usr/bin/fdude
	$(INSTALL_BIN) $(PKG_BUILD_DIR)/restful.lua $(1)/usr/bin/restful
endef

$(eval $(call BuildPackage,flukso))
