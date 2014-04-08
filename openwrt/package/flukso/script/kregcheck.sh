#!/bin/sh
#
#  kregcheck - Check the remote kube registry sources.
#
#  Copyright (C) 2014 Bart Van Der Meerssche <bart@flukso.net>
#
#  This program is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

CACHE_DIR=$(uci get kube.main.cache)
STAGING_DIR=/tmp/kube/staging
URL_LIST=$(uci get kube.main.registry_url)
INIT=0

for URL in $URL_LIST;
do
	REG_NAME=$(echo $URL | sed 's|^.*//||;s|/|.|g')
	REG_DIR=$CACHE_DIR/remote/$REG_NAME
	STAGING_REG_DIR=$STAGING_DIR/$REG_NAME

	mkdir -p $STAGING_REG_DIR
	wget -q -O '-' $URL | tar -xz -C $STAGING_REG_DIR &> /dev/null
	WGET=$(echo $?)
	[ $WGET -gt 0 ] && continue

	diff -qr $REG_DIR $STAGING_REG_DIR &> /dev/null
	DIFF=$(echo $?)
	[ $DIFF -eq 0 ] && continue
	rm -rf $REG_DIR
	cp -r $STAGING_REG_DIR $REG_DIR
	INIT=1
done

rm -rf $STAGING_DIR
[ $INIT -eq 1 ] && ubus send flukso.kube.event '{"event": "e_init"}'
