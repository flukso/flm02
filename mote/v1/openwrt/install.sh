#!/usr/bin/env bash

if (( $# < 1 ))
then
  printf "%b" "Error. Not enough arguments.\n"
  printf "Usage: ./install <your/desired/path/to/backfire>\n"
  exit 1
elif (( $# > 1 ))
then
  printf "%b" "Error. Too many arguments.\n"
  printf "Usage: ./install <your/desired/path/to/backfire>\n"
  exit 2
fi

REPO_PATH=$(pwd)
BACKFIRE_PATH=$1

mkdir -p $BACKFIRE_PATH
svn co svn://svn.openwrt.org/openwrt/branches/backfire $BACKFIRE_PATH

echo "src-link flukso $REPO_PATH/package" > $BACKFIRE_PATH/feeds.conf
$BACKFIRE_PATH/scripts/feeds update
$BACKFIRE_PATH/scripts/feeds install -a -p flukso

cp .config $BACKFIRE_PATH
cp -r files $BACKFIRE_PATH

cp patches/300-set.AR2315_RESET_GPIO.to.6.patch $BACKFIRE_PATH/target/linux/atheros/patches-2.6.30
cp patches/310-hotplug_button_jiffies_calc.patch $BACKFIRE_PATH/target/linux/atheros/patches-2.6.30

cd $BACKFIRE_PATH
patch -p0 < $REPO_PATH/patches/100-disable.console.patch
patch -p0 < $REPO_PATH/patches/110-set.console.baud.to.4800.patch

# and to build the flash image
# make -j4
