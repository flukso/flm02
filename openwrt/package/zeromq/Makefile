# Copyright (C) [2011] [TheClashingRocks.org]
#
# Authors: Jiva Nath Bagale <jnbagale@gmail.com>
#
#
# This is free software, licensed under the GNU General Public License v2.
# See /LICENSE for more information.
#
# 
include $(TOPDIR)/rules.mk

PKG_NAME:=zeromq
PKG_VERSION:=2.1.11

PKG_SOURCE:=$(PKG_NAME)-$(PKG_VERSION).tar.gz
PKG_SOURCE_URL:=http://download.zeromq.org/

PKG_INSTALL:=1

include $(INCLUDE_DIR)/package.mk

define Package/libzmq
    MAINTAINER:=jnbagale@gmail.com
    TITLE:=libzmq
    SECTION:=libs
    DEPENDS:=+libstdcpp +libpthread +librt +libuuid
    CATEGORY:=Libraries
    URL:=http://www.zeromq.org/
endef

define Package/libzmq/description
    The library provides light weight messaging services using an intelligent transport layer. 
    It can carry messages across inproc, IPC, TCP and multicast and is faster than TCP for 
    clustered products and supercomputing.
endef


define Build/InstallDev
	$(INSTALL_DIR) \
		$(1)/usr/lib 	\
		$(1)/usr/include \
		$(1)/usr/lib/pkgconfig
	$(CP) \
		$(PKG_INSTALL_DIR)/usr/lib/* \
		$(1)/usr/lib/
	$(CP) \
		$(PKG_INSTALL_DIR)/usr/include/* \
		$(1)/usr/include/
	$(CP) \
		$(PKG_INSTALL_DIR)/usr/lib/pkgconfig \
		$(1)/usr/lib/

endef

define Package/libzmq/install
	$(INSTALL_DIR) \
		$(1)/usr/lib 
	$(CP) \
		$(PKG_INSTALL_DIR)/usr/lib/libzmq.so* \
		$(1)/usr/lib/

endef

$(eval $(call BuildPackage,libzmq))
