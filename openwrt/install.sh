#!/usr/bin/env bash

VERSION=$(grep PKG_VERSION:= package/flukso/Makefile | sed 's/.*:=[0-9]\.//')
USAGE="Usage: . ./install /your/preferred/backfire/installation/path"

if (( $# < 1 ))
then
	echo "Error. Not enough arguments."
	echo $USAGE
	exit 1
elif (( $# > 1 ))
then
	echo "Error. Too many arguments."
	echo $USAGE
	exit 2
elif [ $1 == "--help" ]
then
	echo $USAGE
	exit 3
fi

REPO_PATH=$(pwd)
INSTALL_PATH=$1/flm02.$VERSION

# checkout the stock OpenWRT build environment [Attitude Adjustment] to the path specified on the command line
mkdir -p $INSTALL_PATH
svn co svn://svn.openwrt.org/openwrt/branches/attitude_adjustment $INSTALL_PATH

# add specific flukso packages as a feed
echo "src-link flukso $REPO_PATH/package" > $INSTALL_PATH/feeds.conf
$INSTALL_PATH/scripts/feeds update
$INSTALL_PATH/scripts/feeds install -a -p flukso

# add patches to the atheros target
cp patches/300-set_AR2315_RESET_GPIO_to_6.patch $INSTALL_PATH/target/linux/atheros/patches-3.3
cp patches/310-register_gpio_leds.patch $INSTALL_PATH/target/linux/atheros/patches-3.3
cp patches/320-flm_spi_platform_support.patch $INSTALL_PATH/target/linux/atheros/patches-3.3
cp patches/330-export_spi_rst_gpio_to_userspace.patch $INSTALL_PATH/target/linux/atheros/patches-3.3
cp patches/340-tune_spi_bitbanging_for_avr.patch $INSTALL_PATH/target/linux/atheros/patches-3.3
cp patches/500-early_printk_disable.patch $INSTALL_PATH/target/linux/atheros/patches-3.3

# patch the default OpenWRT Lua package
cp patches/600-lua-tablecreate.patch $INSTALL_PATH/package/lua/patches

# copy over the build config settings and the files directory
cp .config $INSTALL_PATH
cp -r files $INSTALL_PATH

# copy flash utility to the tools dir
cp ../tools/ap51-flash $INSTALL_PATH/tools

# patch the OpenWRT build system with 900-series patches
cd $INSTALL_PATH

for file in $REPO_PATH/patches/9*.patch
do
	patch -p0 < $file
done
