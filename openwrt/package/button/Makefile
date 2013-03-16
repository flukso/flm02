# Copyright (c) 2010 flukso.net

include $(TOPDIR)/rules.mk

PKG_NAME:=button
PKG_VERSION:=1.0
PKG_RELEASE:=1

PKG_BUILD_DIR:=$(BUILD_DIR)/$(PKG_NAME)-$(PKG_VERSION)

include $(INCLUDE_DIR)/package.mk

define Package/button
  SECTION:=utils
  CATEGORY:=Utilities
  DEPENDS:=
  TITLE:=Button
endef

define Package/button/description
	Helper bash scripts used as a callback for hotplug button events. net_toggle toggles between ethernet and wifi mode. net_defaults reverts all firewall, network and wireless settings to factory defaults. 
endef

define Build/Prepare
	mkdir -p $(PKG_BUILD_DIR)
	$(CP) ./src/* $(PKG_BUILD_DIR)/
endef

define Build/Compile
endef

define Package/button/install
	$(INSTALL_DIR) $(1)/usr/sbin
	$(INSTALL_BIN) $(PKG_BUILD_DIR)/net_toggle $(1)/usr/sbin/
	$(INSTALL_BIN) $(PKG_BUILD_DIR)/net_defaults $(1)/usr/sbin/
	$(INSTALL_BIN) $(PKG_BUILD_DIR)/firmware_defaults $(1)/usr/sbin/
endef

$(eval $(call BuildPackage,button))
