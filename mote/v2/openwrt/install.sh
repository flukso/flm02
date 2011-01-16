#!/usr/bin/env bash

USAGE="Usage: ./install </your/preferred/backfire/installation/path>"

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
BACKFIRE_PATH=$1

# checkout the stock OpenWRT build environment to the path specified on the command line
mkdir -p $BACKFIRE_PATH
svn co svn://svn.openwrt.org/openwrt/branches/backfire $BACKFIRE_PATH

# add the specific flukso packages as a feed
echo "src-link flukso $REPO_PATH/package" > $BACKFIRE_PATH/feeds.conf
$BACKFIRE_PATH/scripts/feeds update
$BACKFIRE_PATH/scripts/feeds install -a -p flukso

# copy over the build config settings and the files directory
cp .config $BACKFIRE_PATH
cp -r files $BACKFIRE_PATH

# add patches to the linux atheros target
cp patches/300-set_AR2315_RESET_GPIO_to_6.patch $BACKFIRE_PATH/target/linux/atheros/patches-2.6.30
cp patches/310-hotplug_button_jiffies_calc.patch $BACKFIRE_PATH/target/linux/atheros/patches-2.6.30
cp patches/400-spi_gpio_support.patch $BACKFIRE_PATH/target/linux/atheros/patches-2.6.30
cp patches/410-spi_gpio_enable_cs_line.patch $BACKFIRE_PATH/target/linux/atheros/patches-2.6.30
cp patches/420-tune_spi_bitbanging_for_avr.patch $BACKFIRE_PATH/target/linux/atheros/patches-2.6.30

# patch files of the OpenWRT build system
cd $BACKFIRE_PATH
patch -p0 < $REPO_PATH/patches/900-disable_console.patch
patch -p0 < $REPO_PATH/patches/910-set_ttyS0_baud_to_115200.patch

# and then build the Fluksometer firmware...
echo 
echo =================================================================================
echo To compile this custom backfire build for Flukso, just type: make -j8
echo Use at least as many jobs as the number of cores available on your build machine.
echo =================================================================================
echo 
