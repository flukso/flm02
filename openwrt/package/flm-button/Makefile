# Copyright (c) 2013 flukso.net

include $(TOPDIR)/rules.mk

PKG_NAME:=flm-button
PKG_VERSION:=1.0
PKG_RELEASE:=1

PKG_BUILD_DIR:=$(BUILD_DIR)/$(PKG_NAME)-$(PKG_VERSION)

include $(INCLUDE_DIR)/package.mk

define Package/flm-button
  SECTION:=flukso
  CATEGORY:=Flukso
  DEPENDS:=
  TITLE:=Fluksometer button event handlers
endef

define Package/flm-button/description
	Helper bash scripts used as a callback for hotplug button events. net_toggle toggles between ethernet and wifi mode. net_defaults reverts all firewall, network and wireless settings to factory defaults. 
endef

define Build/Prepare
	mkdir -p $(PKG_BUILD_DIR)
	$(CP) ./src/* $(PKG_BUILD_DIR)/
endef

define Build/Compile
endef

define Package/flm-button/install
	$(INSTALL_DIR) $(1)/usr/sbin
	$(INSTALL_BIN) $(PKG_BUILD_DIR)/net_toggle $(1)/usr/sbin/
	$(INSTALL_BIN) $(PKG_BUILD_DIR)/net_defaults $(1)/usr/sbin/
	$(INSTALL_BIN) $(PKG_BUILD_DIR)/firmware_defaults $(1)/usr/sbin/
endef

$(eval $(call BuildPackage,flm-button))
