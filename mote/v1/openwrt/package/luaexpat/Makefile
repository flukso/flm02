# 
# Copyright (C) 2006 OpenWrt.org
#
# This is free software, licensed under the GNU General Public License v2.
# See /LICENSE for more information.
#
# $Id: Makefile 6994 2007-04-18 00:46:40Z nico $

include $(TOPDIR)/rules.mk

PKG_NAME:=luaexpat
PKG_VERSION:=1.0.2
PKG_RELEASE:=2

PKG_SOURCE:=$(PKG_NAME)-$(PKG_VERSION).tar.gz
PKG_SOURCE_URL:=http://luaforge.net/frs/download.php/1506/
PKG_MD5SUM:=e99892c4b6b879ae649a491a07c3bed6
PKG_CAT:=zcat

PKG_BUILD_DIR:=$(BUILD_DIR)/$(PKG_NAME)-$(PKG_VERSION)
PKG_INSTALL_DIR:=$(PKG_BUILD_DIR)/ipkg-install

include $(INCLUDE_DIR)/package.mk

define Package/liblua-expat
  SECTION:=lang
  SUBMENU:=LUA
  CATEGORY:=Languages
  TITLE:=luaexpat library for the LUA programming language
  URL:=http://www.luaforge.net/projects/luaexpat
  DEPENDS:=+liblua +libexpat
endef

define Package/liblua-expat/description
	This package contains the luaexpat library, a binding for the expat XML parser for lua.
endef

define Build/Configure
endef

define Build/Compile
	cd $(PKG_BUILD_DIR)/ && \
	$(TARGET_CROSS)gcc -shared -o luaexpat.so src/lxplib.c -Wall -fPIC --std=c99 \
		$(EXTRA_CFLAGS) $(EXTRA_LDFLAGS) \
		$(TARGET_CFLAGS) $(TARGET_LDFLAGS) \
		-I$(STAGING_DIR)/usr/include \
		-lexpat -llua -lm -ldl &&\
	$(TARGET_CROSS)strip luaexpat.so &&\
	$(TARGET_CROSS)gcc -c src/lxplib.c -Wall --std=c99 \
		$(EXTRA_CFLAGS) $(EXTRA_LDFLAGS) \
		$(TARGET_CFLAGS) $(TARGET_LDFLAGS) \
		-I$(STAGING_DIR)/usr/include &&\
	$(TARGET_CROSS)ar r liblua-expat.a lxplib.o
endef

define Build/InstallDev
	mkdir -p $(STAGING_DIR)/usr/include
	mkdir -p $(STAGING_DIR)/usr/lib/lua
	mkdir -p $(STAGING_DIR)/usr/share/lua/lxp
	$(CP) $(PKG_BUILD_DIR)/src/lxplib.h $(STAGING_DIR)/usr/include
	$(CP) $(PKG_BUILD_DIR)/src/lxp/lom.lua $(STAGING_DIR)/usr/share/lua/lxp/
	$(CP) $(PKG_BUILD_DIR)/luaexpat.so \
		$(STAGING_DIR)/usr/lib/liblua-expat.so.0.0.0
	cd $(STAGING_DIR)/usr/lib/ && \
		ln -fs liblua-expat.so.0.0.0 liblua-expat.so
	cd $(STAGING_DIR)/usr/lib/ && \
		ln -fs liblua-expat.so.0.0.0 liblua-expat.so.0
	cd $(STAGING_DIR)/usr/lib/lua/ && \
		ln -fs ../liblua-expat.so.0.0.0 lxp.so
	$(CP) $(PKG_BUILD_DIR)/liblua-expat.a $(STAGING_DIR)/usr/lib
endef

define Build/UninstallDev
	$(RM) -f $(STAGING_DIR)/usr/include/lxplib.h
	$(RM) -f $(STAGING_DIR)/usr/lib/liblua-expat.so.0.0.0
	$(RM) -f $(STAGING_DIR)/usr/lib/liblua-expat.so.0
	$(RM) -f $(STAGING_DIR)/usr/lib/liblua-expat.a
	$(RM) -f $(STAGING_DIR)/usr/lib/lua/lxp.so
	$(RM) -rf $(STAGING_DIR)/usr/share/lua/lxp/
endef

define Package/liblua-expat/install
	$(INSTALL_DIR) $(1)/usr/lib/lua
	$(INSTALL_DIR) $(1)/usr/share/lua/lxp
	$(CP) $(PKG_BUILD_DIR)/luaexpat.so \
		$(1)/usr/lib/liblua-expat.so.0.0.0
	cd $(1)/usr/lib/lua/ && \
		ln -fs ../liblua-expat.so.0.0.0 lxp.so
	cd $(1)/usr/lib/ && ln -sf liblua-expat.so.0.0.0 liblua-expat.so
	$(CP) $(PKG_BUILD_DIR)/src/lxp/lom.lua $(1)/usr/share/lua/lxp/
endef

$(eval $(call BuildPackage,liblua-expat))
