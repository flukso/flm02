#!/bin/sh
#
#  fcheck - Check whether the Flukso daemon components are still running.
#
#  Copyright (C) 2011 Bart Van Der Meerssche <bart.vandermeerssche@flukso.net>
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

[ -z "$(ps | grep 'spi[d]')" ] && exit 1
[ -z "$(ps | grep 'flukso[d]')" ] && exit 2
[ -z "$(ps | grep 'parse[d]')" ] && exit 3

exit 0
