#!/bin/sh
#
#  fcheck - check Flukso daemons
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

STATUS=0

[ -z "$(ps | grep 'spi[d]')" ] && STATUS=1
[ -z "$(ps | grep 'flukso[d]')" ] && STATUS=2

MODEL=$(uci get system.@system[0].model)

if [ $MODEL == FLM02B -o $MODEL == FLM02C ]
then
	[ -z "$(ps | grep 'parse[d]')" ] && STATUS=3
	[ -z "$(ps | grep 'kube[d]')" ] && STATUS=4
fi

[ -z "$(ps | grep 'sup[d]')" ] && STATUS=5

[ $STATUS -ne 0 ] && /etc/init.d/flukso restart
