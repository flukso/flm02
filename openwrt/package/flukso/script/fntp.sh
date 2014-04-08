#!/bin/sh
#
#  fntp - Make sure ntp deamon is running when iface is up.
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

[ -z "$(ps | grep 'ntp[d]')" -o $(date '+%s') -lt 1234567890 ] \
	&& { logger ntpd needed a kicking; /etc/init.d/sysntpd stop; /etc/init.d/sysntpd start; }

exit 0
