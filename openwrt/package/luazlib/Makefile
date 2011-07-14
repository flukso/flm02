#
# Copyright (C) 2011 OpenWrt.org
#
# This is free software, licensed under the GNU General Public License v2.
# See /LICENSE for more information.
#

include $(TOPDIR)/rules.mk

PKG_NAME:=luazlib
PKG_VERSION:=0.2
PKG_RELEASE:=1
PKG_CMT:=8d076690cd84702d051a548740dfa0f2102dc28e

PKG_SOURCE:=$(PKG_NAME)-$(PKG_VERSION).tar.gz
PKG_SOURCE_URL:=git://github.com/brimworks/lua-zlib.git
PKG_SOURCE_SUBDIR:=$(PKG_NAME)-$(PKG_VERSION)
PKG_SOURCE_VERSION:=$(PKG_CMT)
PKG_SOURCE_PROTO:=git

PKG_BUILD_DIR:=$(BUILD_DIR)/$(PKG_SOURCE_SUBDIR)

include $(INCLUDE_DIR)/package.mk

define Package/luazlib
  SUBMENU:=Lua
  SECTION:=lang
  CATEGORY:=Languages
  TITLE:=Lua-zlib
  URL:=https://github.com/brimworks/lua-zlib
  DEPENDS:=+lua +zlib
  MAINTAINER:=Bart Van Der Meerssche <bart.vandermeerssche@flukso.net>
endef

define Package/luazlib/description
  Lua-zlib is a simple Lua binding to zlib.
endef

TARGET = lua_zlib
LIBS = -ldl -lm -lz -llua
TARGET_CFLAGS += $(FPIC)
TARGET_LDFLAGS += -shared

define Build/Compile
	$(TARGET_CC) $(TARGET_CFLAGS) \
		-I$(STAGING_DIR)/usr/include \
		-c $(PKG_BUILD_DIR)/$(TARGET).c \
		-o $(PKG_BUILD_DIR)/$(TARGET).o
	$(TARGET_CC) $(TARGET_LDFLAGS) \
		$(LIBS) \
		$(PKG_BUILD_DIR)/$(TARGET).o \
		-o $(PKG_BUILD_DIR)/zlib.so
endef

define Package/luazlib/install
	$(INSTALL_DIR) $(1)/usr/lib/lua
	$(INSTALL_BIN) $(PKG_BUILD_DIR)/zlib.so $(1)/usr/lib/lua/
endef

$(eval $(call BuildPackage,luazlib))
