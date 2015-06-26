include $(TOPDIR)/rules.mk

PKG_NAME:=flm-ui
PKG_VERSION:=2015-06-26
PKG_RELEASE=$(PKG_SOURCE_VERSION)

PKG_SOURCE_PROTO:=git
PKG_SOURCE_URL:=git://github.com/flukso/$(PKG_NAME).git
PKG_SOURCE_SUBDIR:=$(PKG_NAME)-$(PKG_VERSION)
PKG_SOURCE_VERSION:=3124743b19372b0ac7cdf814fbf86aa2df587297
PKG_SOURCE:=$(PKG_NAME)-$(PKG_VERSION)-$(PKG_SOURCE_VERSION).tar.gz

PKG_MAINTAINER:=Bart Van Der Meerssche <bart@flukso.net>

include $(INCLUDE_DIR)/package.mk

define Package/flm-ui
    SECTION:=flukso
    CATEGORY:=Flukso
    TITLE:=Fluksometer UI
    DEPENDS:=+luci-mod-rpc
endef

define Package/flm-ui/description
	A fresh UI for the Fluksometer
endef

define Package/flm-ui/install
	$(INSTALL_DIR) $(1)/www
	$(INSTALL_DATA) $(PKG_BUILD_DIR)/app/index.html $(1)/www/
	$(INSTALL_DIR) $(1)/www/components/angular
	$(INSTALL_DATA) $(PKG_BUILD_DIR)/app/components/angular/angular.min.js \
		$(1)/www/components/angular
	$(INSTALL_DIR) $(1)/www/components/angular-route
	$(INSTALL_DATA) $(PKG_BUILD_DIR)/app/components/angular-route/angular-route.min.js \
		$(1)/www/components/angular-route
	$(INSTALL_DIR) $(1)/www/components/angular-bootstrap
	$(INSTALL_DATA) $(PKG_BUILD_DIR)/app/components/angular-bootstrap/ui-bootstrap-tpls.min.js \
		$(1)/www/components/angular-bootstrap
	$(INSTALL_DIR) $(1)/www/components/mqttws
	$(INSTALL_DATA) $(PKG_BUILD_DIR)/app/components/mqttws/mqttws31-min.js \
		$(1)/www/components/mqttws
	$(INSTALL_DIR) $(1)/www/img
	$(INSTALL_DATA) $(PKG_BUILD_DIR)/app/img/flukso.logo.png $(1)/www/img
	$(INSTALL_DIR) $(1)/www/styles
	$(INSTALL_DATA) $(PKG_BUILD_DIR)/app/styles/{bootstrap,main}.css $(1)/www/styles
	$(INSTALL_DIR) $(1)/www/views
	$(INSTALL_DATA) $(PKG_BUILD_DIR)/app/views/{main,sensor,wifi,status,services,syslog,mqtt,kube}.html \
		$(1)/www/views
	$(INSTALL_DIR) $(1)/www/scripts/controllers
	$(INSTALL_DATA) $(PKG_BUILD_DIR)/app/scripts/app.js $(1)/www/scripts
	$(INSTALL_DATA) $(PKG_BUILD_DIR)/app/scripts/controllers/{main,sensor,wifi,status,services,syslog,mqtt,kube}.js \
		$(1)/www/scripts/controllers
endef

$(eval $(call BuildPackage,flm-ui))
