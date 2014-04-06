#
# Copyright (C) 2014 OpenWrt.org
#
# This is free software, licensed under the GNU General Public License v2.
# See /LICENSE for more information.
#

include $(TOPDIR)/rules.mk

PKG_NAME:=rFSM
PKG_VERSION:=2013-09-25
PKG_RELEASE=$(PKG_SOURCE_VERSION)

PKG_SOURCE_PROTO:=git
PKG_SOURCE_URL:=git://github.com/kmarkus/$(PKG_NAME).git
PKG_SOURCE_SUBDIR:=$(PKG_NAME)-$(PKG_VERSION)
PKG_SOURCE_VERSION:=b259a614a1c7495047af2a72edcc428ffcbc6b39
PKG_SOURCE:=$(PKG_NAME)-$(PKG_VERSION)-$(PKG_SOURCE_VERSION).tar.gz

PKG_MAINTAINER:=Bart Van Der Meerssche <bart@flukso.net>

include $(INCLUDE_DIR)/package.mk

define Package/rFSM
    SUBMENU:=Lua
    SECTION:=lang
    CATEGORY:=Languages
    TITLE:=rFSM
    DEPENDS:=+lua
endef

define Package/rFSM/description
	rFSM is a lightweight Statechart implementation in Lua
endef

define Build/Compile
endef

define Package/rFSM/install
	$(INSTALL_DIR) $(1)/usr/lib/lua/rfsm
	$(INSTALL_DATA) $(PKG_BUILD_DIR)/rfsm.lua $(1)/usr/lib/lua/rfsm/init.lua
	$(INSTALL_DATA) $(PKG_BUILD_DIR)/{ansicolors,time,utils}.lua \
		$(1)/usr/lib/lua/rfsm
	$(INSTALL_DATA) $(PKG_BUILD_DIR)/rfsmpp.lua \
		$(1)/usr/lib/lua/rfsm/pp.lua

	for module in marsh await checkevents emem preview timeevent; do \
		$(INSTALL_DATA) $(PKG_BUILD_DIR)/rfsm_"$$$$module".lua \
			$(1)/usr/lib/lua/rfsm/"$$$$module".lua; \
	done
endef

$(eval $(call BuildPackage,rFSM))
