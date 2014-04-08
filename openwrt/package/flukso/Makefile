#
# Copyright (C) 2014 OpenWrt.org
#
# This is free software, licensed under the GNU General Public License v2.
# See /LICENSE for more information.
#

include $(TOPDIR)/rules.mk

PKG_NAME:=flukso
PKG_VERSION:=2.4.0
PKG_RELEASE:=alpha

PKG_BUILD_DIR:=$(BUILD_DIR)/$(PKG_NAME)-$(PKG_VERSION)

include $(INCLUDE_DIR)/package.mk

define Package/flukso
	SECTION:=flukso
	CATEGORY:=Flukso
	DEPENDS:=+libdaemon +liblua +luci-lib-nixio +luci-lib-core \
		+luci-lib-httpclient +luci-lib-json +luci-lib-sys \
		+libubox-lua +libubus-lua +rFSM +vstruct
	TITLE:=Fluksometer daemon collection
	URL:=http://www.flukso.net
	MAINTAINER:=Bart Van Der Meerssche <bart@flukso.net>
endef

define Package/flukso/description
	This package contains the Flukso daemon code and support libraries.
endef

define Build/Prepare
	mkdir -p $(PKG_BUILD_DIR)
	$(CP) ./src/* $(PKG_BUILD_DIR)/

	make -C ../../../avr/a clean
	make -C ../../../avr/a
	make -C ../../../avr/b clean
	make -C ../../../avr/b
endef

FLUKSO_COMMIT:=$(shell git rev-parse HEAD | awk '{print substr($$0,1,16)}')

define Package/flukso/install
	$(CP) ./files/* $(1)/
	
	$(SED) 's,$$$$R,$(REVISION),g' $(1)/etc/banner
	$(SED) 's,$$$$F,$(PKG_VERSION)-$(PKG_RELEASE),g' $(1)/etc/banner
	$(SED) 's,$$$$G,$(FLUKSO_COMMIT),g' $(1)/etc/banner

	$(INSTALL_DIR) $(1)/etc/config
	for file in flukso kube; do \
		$(INSTALL_DATA) ./config/"$$$$file".uci $(1)/etc/config/"$$$$file"; \
		chmod a+w $(1)/etc/config/"$$$$file"; \
	done
	$(INSTALL_DIR) $(1)/etc/init.d
	$(INSTALL_BIN) ./config/flukso.init $(1)/etc/init.d/flukso

	$(INSTALL_DIR) $(1)/usr/lib/lua
	$(CP) ./luasrc/{dbg.lua,flukso} $(1)/usr/lib/lua/

	$(INSTALL_DIR) $(1)/usr/sbin
	$(INSTALL_BIN) $(PKG_BUILD_DIR)/luad $(1)/usr/sbin/
	for link in fluksod spid parsed kubed supd; do \
		$(LN) /usr/sbin/luad $(1)/usr/sbin/"$$$$link"; \
	done
	$(INSTALL_BIN) ./luasrc/{fluksod,spid,parsed,kubed,supd}.lua $(1)/usr/sbin/

	$(INSTALL_DIR) $(1)/usr/bin
	for file in heartbeat fsync fdude fbackup ftest \
				fp1 ftic restful; do \
		$(INSTALL_BIN) ./luasrc/"$$$$file".lua $(1)/usr/bin/"$$$$file"; \
	done
	for file in fcheck fntp kregcheck; do \
		$(INSTALL_BIN) ./script/"$$$$file".sh $(1)/usr/bin/"$$$$file"; \
	done

	$(INSTALL_DIR) $(1)/usr/bin/avr/{a,b}
	$(INSTALL_DATA) ../../../avr/a/*.{hex,eep} $(1)/usr/bin/avr/a/
	$(INSTALL_DATA) ../../../avr/b/*.{hex,eep} $(1)/usr/bin/avr/b/
endef

$(eval $(call BuildPackage,flukso))
