# 
# Copyright (C) 2006 OpenWrt.org
#
# This is free software, licensed under the GNU General Public License v2.
# See /LICENSE for more information.
#
# $Id: Makefile 6994 2007-04-18 00:46:40Z nico $

include $(TOPDIR)/rules.mk

PKG_NAME:=luasocket
PKG_VERSION:=2.0.2
PKG_VERSION_MAJOR:=$(shell echo $(PKG_VERSION) | cut -d . -f 1)
PKG_RELEASE:=1

PKG_SOURCE:=$(PKG_NAME)-$(PKG_VERSION).tar.gz
PKG_SOURCE_URL:=http://luaforge.net/frs/download.php/2664/
PKG_MD5SUM:=41445b138deb7bcfe97bff957503da8e
PKG_CAT:=zcat

PKG_BUILD_DIR:=$(BUILD_DIR)/$(PKG_NAME)-$(PKG_VERSION)
PKG_INSTALL_DIR:=$(PKG_BUILD_DIR)/ipkg-install

include $(INCLUDE_DIR)/package.mk

define Package/liblua-socket
  SECTION:=lang
  SUBMENU:=LUA
  CATEGORY:=Languages
  DEPENDS:=+liblua
  URL:=http://luasocket.luaforge.net/
  TITLE:=TCP/UDP socket library for LUA programming language
endef

define Package/liblua-socket/description
        Luasocket provides a TCP/UDP socket library for Lua 5.1. Also provided are HTTP and SMTP implementations, a simple FTP implementation and some URL parsing functions. Luasocket supports blocking and non-blocking sockets and also provides a select() interface to the language.

endef

define Build/Configure
endef

# omitted static libraries
define Build/Compile
	cd $(PKG_BUILD_DIR)/ && \
	$(TARGET_CROSS)gcc -shared -o src/socket.so \
		src/luasocket.c \
		src/timeout.c \
		src/buffer.c \
		src/io.c \
		src/auxiliar.c \
		src/options.c \
		src/inet.c \
		src/tcp.c \
		src/udp.c \
		src/except.c \
		src/select.c \
		src/usocket.c -Wall -fPIC \
		$(EXTRA_CFLAGS) $(EXTRA_LDFLAGS) \
		$(TARGET_CFLAGS) $(TARGET_LDFLAGS) \
		-I$(STAGING_DIR)/usr/include \
		-llua -lm -ldl &&\
        $(TARGET_CROSS)strip src/socket.so &&\
	$(TARGET_CROSS)gcc -shared -o src/mime.so \
		src/mime.c -Wall -fPIC \
		$(EXTRA_CFLAGS) $(EXTRA_LDFLAGS) \
		$(TARGET_CFLAGS) $(TARGET_LDFLAGS) \
		-I$(STAGING_DIR)/usr/include \
		-llua -lm -ldl &&\
        $(TARGET_CROSS)strip src/mime.so &&\
	$(TARGET_CROSS)gcc -shared -o src/unix.so \
		src/buffer.c \
		src/auxiliar.c \
		src/options.c \
		src/timeout.c \
		src/io.c \
		src/usocket.c \
		src/unix.c -Wall -fPIC \
		$(EXTRA_CFLAGS) $(EXTRA_LDFLAGS) \
		$(TARGET_CFLAGS) $(TARGET_LDFLAGS) \
		-I$(STAGING_DIR)/usr/include \
		-llua -lm -ldl &&\
        $(TARGET_CROSS)strip src/unix.so
endef

# omitted static libraries
define Build/InstallDev
	mkdir -p $(STAGING_DIR)/usr/include/lua
	mkdir -p $(STAGING_DIR)/usr/lib/lua/mime
	mkdir -p $(STAGING_DIR)/usr/lib/lua/socket
	mkdir -p $(STAGING_DIR)/usr/share/lua/socket
	$(CP) $(PKG_BUILD_DIR)/src/ltn12.lua $(STAGING_DIR)/usr/share/lua/
	$(CP) $(PKG_BUILD_DIR)/src/mime.lua $(STAGING_DIR)/usr/share/lua/
	$(CP) $(PKG_BUILD_DIR)/src/socket.lua $(STAGING_DIR)/usr/share/lua/
	$(CP) $(PKG_BUILD_DIR)/src/ftp.lua $(STAGING_DIR)/usr/share/lua/socket/
	$(CP) $(PKG_BUILD_DIR)/src/http.lua $(STAGING_DIR)/usr/share/lua/socket/
	$(CP) $(PKG_BUILD_DIR)/src/smtp.lua $(STAGING_DIR)/usr/share/lua/socket/
	$(CP) $(PKG_BUILD_DIR)/src/tp.lua $(STAGING_DIR)/usr/share/lua/socket/
	$(CP) $(PKG_BUILD_DIR)/src/url.lua $(STAGING_DIR)/usr/share/lua/socket/
	$(CP) $(PKG_BUILD_DIR)/src/socket.h $(STAGING_DIR)/usr/include/lua
	$(CP) $(PKG_BUILD_DIR)/src/socket.so \
		$(STAGING_DIR)/usr/lib/liblua-socket.so.$(PKG_VERSION)
	cd $(STAGING_DIR)/usr/lib/ && \
		ln -fs liblua-socket.so.$(PKG_VERSION) liblua-socket.so
	cd $(STAGING_DIR)/usr/lib/ && \
		ln -fs liblua-socket.so.$(PKG_VERSION) liblua-socket.so.$(PKG_VERSION_MAJOR)
	cd $(STAGING_DIR)/usr/lib/lua/socket && \
		ln -fs ../../liblua-socket.so.$(PKG_VERSION) core.so
	$(CP) $(PKG_BUILD_DIR)/src/mime.h $(STAGING_DIR)/usr/include/lua
	$(CP) $(PKG_BUILD_DIR)/src/mime.so \
		$(STAGING_DIR)/usr/lib/liblua-socket-mime.so.$(PKG_VERSION)
	cd $(STAGING_DIR)/usr/lib/ && \
		ln -fs liblua-socket-mime.so.$(PKG_VERSION) liblua-socket-mime.so
	cd $(STAGING_DIR)/usr/lib/ && \
		ln -fs liblua-socket-mime.so.$(PKG_VERSION) liblua-socket-mime.so.$(PKG_VERSION_MAJOR)
	cd $(STAGING_DIR)/usr/lib/lua/mime && \
		ln -fs ../../liblua-socket-mime.so.$(PKG_VERSION) core.so
	$(CP) $(PKG_BUILD_DIR)/src/unix.h $(STAGING_DIR)/usr/include/lua
	$(CP) $(PKG_BUILD_DIR)/src/unix.so \
		$(STAGING_DIR)/usr/lib/liblua-socket-unix.so.$(PKG_VERSION)
	cd $(STAGING_DIR)/usr/lib/ && \
		ln -fs liblua-socket-unix.so.$(PKG_VERSION) liblua-socket-unix.so
	cd $(STAGING_DIR)/usr/lib/ && \
		ln -fs liblua-socket-unix.so.$(PKG_VERSION) liblua-socket-unix.so.$(PKG_VERSION_MAJOR)
	cd $(STAGING_DIR)/usr/lib/lua/socket && \
		ln -fs ../../liblua-socket-unix.so.$(PKG_VERSION) unix.so
endef

define Build/UninstallDev
	$(RM) -f $(STAGING_DIR)/usr/share/lua/ltn12.lua
	$(RM) -f $(STAGING_DIR)/usr/share/lua/mime.lua
	$(RM) -f $(STAGING_DIR)/usr/share/lua/socket.lua
	$(RM) -f $(STAGING_DIR)/usr/share/lua/socket/ftp.lua
	$(RM) -f $(STAGING_DIR)/usr/share/lua/socket/http.lua
	$(RM) -f $(STAGING_DIR)/usr/share/lua/socket/smtp.lua
	$(RM) -f $(STAGING_DIR)/usr/share/lua/socket/tp.lua
	$(RM) -f $(STAGING_DIR)/usr/share/lua/socket/url.lua
	$(RM) -f $(STAGING_DIR)/usr/include/lua/socket.h
	$(RM) -f $(STAGING_DIR)/usr/lib/liblua-socket.so.$(PKG_VERSION)
	$(RM) -f $(STAGING_DIR)/usr/lib/liblua-socket.so.$(PKG_VERSION_MAJOR)
	$(RM) -f $(STAGING_DIR)/usr/lib/lua/socket/core.so
	$(RM) -f $(STAGING_DIR)/usr/include/lua/mime.h
	$(RM) -f $(STAGING_DIR)/usr/lib/liblua-socket-mime.so.$(PKG_VERSION)
	$(RM) -f $(STAGING_DIR)/usr/lib/liblua-socket-mime.so.$(PKG_VERSION_MAJOR)
	$(RM) -f $(STAGING_DIR)/usr/lib/lua/mime/core.so
	$(RM) -f $(STAGING_DIR)/usr/include/lua/unix.h
	$(RM) -f $(STAGING_DIR)/usr/lib/liblua-socket-unix.so.$(PKG_VERSION)
	$(RM) -f $(STAGING_DIR)/usr/lib/liblua-socket-unix.so.$(PKG_VERSION_MAJOR)
	$(RM) -f $(STAGING_DIR)/usr/lib/lua/socket/unix.so
endef

define Package/liblua-socket/install
	$(INSTALL_DIR) $(1)/usr/lib/lua/mime
	$(INSTALL_DIR) $(1)/usr/lib/lua/socket
	$(INSTALL_DIR) $(1)/usr/share/lua/socket
	$(CP) $(PKG_BUILD_DIR)/src/ltn12.lua $(1)/usr/share/lua/
	$(CP) $(PKG_BUILD_DIR)/src/mime.lua $(1)/usr/share/lua/
	$(CP) $(PKG_BUILD_DIR)/src/socket.lua $(1)/usr/share/lua/
	$(CP) $(PKG_BUILD_DIR)/src/ftp.lua $(1)/usr/share/lua/socket/
	$(CP) $(PKG_BUILD_DIR)/src/http.lua $(1)/usr/share/lua/socket/
	$(CP) $(PKG_BUILD_DIR)/src/smtp.lua $(1)/usr/share/lua/socket/
	$(CP) $(PKG_BUILD_DIR)/src/tp.lua $(1)/usr/share/lua/socket/
	$(CP) $(PKG_BUILD_DIR)/src/url.lua $(1)/usr/share/lua/socket/
	$(CP) $(PKG_BUILD_DIR)/src/socket.so \
		$(1)/usr/lib/liblua-socket.so.$(PKG_VERSION)
	cd $(1)/usr/lib/ && \
		ln -fs liblua-socket.so.$(PKG_VERSION) liblua-socket.so
	cd $(1)/usr/lib/lua/socket && \
		ln -fs ../../liblua-socket.so.$(PKG_VERSION) core.so
	$(CP) $(PKG_BUILD_DIR)/src/mime.so \
		$(1)/usr/lib/liblua-socket-mime.so.$(PKG_VERSION)
	cd $(1)/usr/lib/ && \
		ln -fs liblua-socket-mime.so.$(PKG_VERSION) liblua-socket-mime.so
	cd $(1)/usr/lib/lua/mime && \
		ln -fs ../../liblua-socket-mime.so.$(PKG_VERSION) core.so
	$(CP) $(PKG_BUILD_DIR)/src/unix.so \
		$(1)/usr/lib/liblua-socket-unix.so.$(PKG_VERSION)
	cd $(1)/usr/lib/ && \
		ln -fs liblua-socket-unix.so.$(PKG_VERSION) liblua-socket-unix.so
	cd $(1)/usr/lib/lua/socket && \
		ln -fs ../../liblua-socket-unix.so.$(PKG_VERSION) unix.so
endef

$(eval $(call BuildPackage,liblua-socket))
