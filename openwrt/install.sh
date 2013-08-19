#!/usr/bin/env bash

#VERSION=$(grep PKG_VERSION:= package/flukso/Makefile | sed 's/.*:=[0-9]\.//')
VERSION=3.0
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

# add patches to the atheros target

# copy over the build config settings and the files directory
#cp .config $INSTALL_PATH
#cp -r files $INSTALL_PATH

# patch files of the OpenWRT build system
cd $INSTALL_PATH
