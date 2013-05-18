#
# Copyright (C) 2013 OpenWrt.org
#
# This is free software, licensed under the GNU General Public License v2.
# See /LICENSE for more information.
#

include $(TOPDIR)/rules.mk

PKG_NAME:=flm-ui
PKG_VERSION:=1.0
PKG_RELEASE:=1

PKG_SOURCE:=$(PKG_NAME)-$(PKG_VERSION).tar.bz2
PKG_SOURCE_SUBDIR:=$(PKG_NAME)-$(PKG_VERSION)
PKG_SOURCE_URL:=git://github.com/flukso/$(PKG_NAME).git
PKG_SOURCE_PROTO:=git
PKG_SOURCE_VERSION:=cfda1f5c27a344852c8074cb51827cef54921324
include $(INCLUDE_DIR)/package.mk

define Package/flm-ui
    SECTION:=flukso
    CATEGORY:=Flukso
    TITLE:=Flm-ui
    DEPENDS:=
    MAINTAINER:=Bart Van Der Meerssche <bart@flukso.net>
endef

define Package/flm-ui/description
	A fresh local UI for the Fluksometer
endef

define Package/flm-ui/install
	$(INSTALL_DIR) $(1)/www
	$(INSTALL_DATA) $(PKG_BUILD_DIR)/app/index.html $(1)/www/
	$(INSTALL_DIR) $(1)/www/components/angular
	$(INSTALL_DATA) $(PKG_BUILD_DIR)/app/components/angular/angular.min.js \
		$(1)/www/components/angular
	$(INSTALL_DIR) $(1)/www/components/angular-bootstrap
	$(INSTALL_DATA) $(PKG_BUILD_DIR)/app/components/angular-bootstrap/ui-bootstrap-tpls.min.js \
		$(1)/www/components/angular-bootstrap
	$(INSTALL_DIR) $(1)/www/img
	$(INSTALL_DATA) $(PKG_BUILD_DIR)/app/img/flukso.logo.png $(1)/www/img
	$(INSTALL_DIR) $(1)/www/styles
	$(INSTALL_DATA) $(PKG_BUILD_DIR)/app/styles/{bootstrap,main}.css $(1)/www/styles
	$(INSTALL_DIR) $(1)/www/views
	$(INSTALL_DATA) $(PKG_BUILD_DIR)/app/views/{main,sensor,wifi,status,services,syslog}.html \
		$(1)/www/views
	$(INSTALL_DIR) $(1)/www/scripts/controllers
	$(INSTALL_DATA) $(PKG_BUILD_DIR)/app/scripts/app.js $(1)/www/scripts
	$(INSTALL_DATA) $(PKG_BUILD_DIR)/app/scripts/controllers/{main,sensor,wifi,status,services,syslog}.js \
		$(1)/www/scripts/controllers
endef

$(eval $(call BuildPackage,flm-ui))
