#!/bin/sh
#
#  fntp - Make sure ntpclient deamon is running when iface is up.
#
#  Copyright (C) 2012 Bart Van Der Meerssche <bart.vandermeerssche@flukso.net>
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

. /etc/hotplug.d/iface/20-ntpclient

[ -z "$(ps | grep 'ntpclien[t]')" -o $(date '+%s') -lt 1234567890 ] \
	&& { logger ntpclient needed a kicking; stop_ntpclient; start_ntpclient; }

exit 0
