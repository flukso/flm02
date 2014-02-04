#
# Copyright (C) 2013 OpenWrt.org
#
# This is free software, licensed under the GNU General Public License v2.
# See /LICENSE for more information.
#

include $(TOPDIR)/rules.mk

PKG_NAME:=flukso
PKG_VERSION:=2.3.2
PKG_RELEASE:=alpha

PKG_BUILD_DIR:=$(BUILD_DIR)/$(PKG_NAME)-$(PKG_VERSION)

include $(INCLUDE_DIR)/package.mk

define Package/flukso
	SECTION:=flukso
	CATEGORY:=Flukso
	DEPENDS:=+libdaemon +liblua +luci-lib-nixio +rrdtool1
	TITLE:=Fluksometer daemon
	URL:=http://www.flukso.net
	MAINTAINER:=Bart Van Der Meerssche <bart@flukso.net>
endef

define Package/flukso/description
	This package contains the Flukso daemon code and support libraries.
endef

define Build/Prepare
	mkdir -p $(PKG_BUILD_DIR)
	$(CP) ./src/* $(PKG_BUILD_DIR)/
	$(CP) ./luasrc/* $(PKG_BUILD_DIR)/
	$(CP) ./config/* $(PKG_BUILD_DIR)/
	$(CP) -r ./files $(PKG_BUILD_DIR)/

	make -C ../../../avr/a clean
	make -C ../../../avr/a
	make -C ../../../avr/b clean
	make -C ../../../avr/b
	mkdir -p $(PKG_BUILD_DIR)/avr/{a,b}
	$(CP) ../../../avr/a/*.{hex,eep} $(PKG_BUILD_DIR)/avr/a
	$(CP) ../../../avr/b/*.{hex,eep} $(PKG_BUILD_DIR)/avr/b
endef

FLUKSO_COMMIT:=$(shell git rev-parse HEAD | awk '{print substr($$0,1,16)}')

define Package/flukso/install
	$(INSTALL_DIR) $(1)/etc
	$(INSTALL_DATA) $(PKG_BUILD_DIR)/files/etc/banner $(1)/etc/banner
	
	$(SED) 's,$$$$R,$(REVISION),g' $(1)/etc/banner
	$(SED) 's,$$$$F,$(PKG_VERSION)-$(PKG_RELEASE),g' $(1)/etc/banner
	$(SED) 's,$$$$G,$(FLUKSO_COMMIT),g' $(1)/etc/banner

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
	$(INSTALL_DIR) $(1)/usr/lib/lua/flukso/decoder
	$(INSTALL_DATA) $(PKG_BUILD_DIR)/flukso/decoder/*.lua $(1)/usr/lib/lua/flukso/decoder/

	$(INSTALL_DIR) $(1)/usr/sbin
	$(INSTALL_BIN) $(PKG_BUILD_DIR)/luad $(1)/usr/sbin/
	$(LN) /usr/sbin/luad $(1)/usr/sbin/fluksod
	$(INSTALL_BIN) $(PKG_BUILD_DIR)/fluksod.lua $(1)/usr/sbin/
	$(LN) /usr/sbin/luad $(1)/usr/sbin/spid
	$(INSTALL_BIN) $(PKG_BUILD_DIR)/spid.lua $(1)/usr/sbin/
	$(LN) /usr/sbin/luad $(1)/usr/sbin/parsed
	$(INSTALL_BIN) $(PKG_BUILD_DIR)/parsed.lua $(1)/usr/sbin/
	$(LN) /usr/sbin/luad $(1)/usr/sbin/supd
	$(INSTALL_BIN) $(PKG_BUILD_DIR)/supd.lua $(1)/usr/sbin/
	$(LN) /usr/sbin/luad $(1)/usr/sbin/868_to_mqttd
	$(INSTALL_BIN) $(PKG_BUILD_DIR)/868_to_mqttd.lua $(1)/usr/sbin/

	$(INSTALL_DIR) $(1)/usr/bin
	$(INSTALL_BIN) $(PKG_BUILD_DIR)/heartbeat.lua $(1)/usr/bin/heartbeat
	$(INSTALL_BIN) $(PKG_BUILD_DIR)/fsync.lua $(1)/usr/bin/fsync
	$(INSTALL_BIN) $(PKG_BUILD_DIR)/fdude.lua $(1)/usr/bin/fdude
	$(INSTALL_BIN) $(PKG_BUILD_DIR)/fbackup.lua $(1)/usr/bin/fbackup
	$(INSTALL_BIN) $(PKG_BUILD_DIR)/ftest.lua $(1)/usr/bin/ftest
	$(INSTALL_BIN) $(PKG_BUILD_DIR)/fp1.lua $(1)/usr/bin/fp1
	$(INSTALL_BIN) $(PKG_BUILD_DIR)/ftic.lua $(1)/usr/bin/ftic
	$(INSTALL_BIN) $(PKG_BUILD_DIR)/fcheck.sh $(1)/usr/bin/fcheck
	$(INSTALL_BIN) $(PKG_BUILD_DIR)/fntp.sh $(1)/usr/bin/fntp
	$(INSTALL_BIN) $(PKG_BUILD_DIR)/restful.lua $(1)/usr/bin/restful

	$(INSTALL_DIR) $(1)/usr/bin/avr/a
	$(INSTALL_DIR) $(1)/usr/bin/avr/b
	$(INSTALL_DATA) $(PKG_BUILD_DIR)/avr/a/*.{hex,eep} $(1)/usr/bin/avr/a/
	$(INSTALL_DATA) $(PKG_BUILD_DIR)/avr/b/*.{hex,eep} $(1)/usr/bin/avr/b/
endef

$(eval $(call BuildPackage,flukso))
