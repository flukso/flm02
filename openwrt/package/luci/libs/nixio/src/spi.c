/*
 * nixio - Linux I/O library for lua
 *
 *   Copyright (C) 2011 Bart Van Der Meerssche <bart.vandermeerssche@flukso.net>
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

#include "nixio.h"
#include <errno.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <linux/spi/spidev.h>

/* Defined in linux/spi/spidev.h, but this doesn't seem to propagate to the openwrt staging dir */
/* Read / Write SPI device default delay us */
#define SPI_IOC_RD_DELAY_US		_IOR(SPI_IOC_MAGIC, 5, __u32)
#define SPI_IOC_WR_DELAY_US		_IOW(SPI_IOC_MAGIC, 5, __u32)


static int nixio_spi_setspeed(lua_State *L) {
	int fd = nixio__checkfd(L, 1);
	ulong speed_hz = luaL_checkinteger(L, 2);
	uint delay_usecs = luaL_checkinteger(L, 3);

	if (ioctl(fd, SPI_IOC_WR_MAX_SPEED_HZ, &speed_hz) < 0) {
		return nixio__perror(L);
	} else if (ioctl(fd, SPI_IOC_WR_DELAY_US, &delay_usecs) < 0) {
		return nixio__perror(L);
	}

	return 0;
}

/* not really needed anymore since this is now coded into the spi_bitbang kmod */
static int nixio_spi_read(lua_State *L) {
	int fd = nixio__checkfd(L, 1);
	char buffer[NIXIO_BUFFERSIZE];
	int readc;
	size_t len =0;
	char last = 0;

	for (size_t i = 0; i < NIXIO_BUFFERSIZE; i++) {
		do {
			readc = read(fd, buffer + i, 1);
		} while (readc == -1 && errno == EINTR);

		if (readc < 0) {
			return nixio__perror(L);
		}

		if (last) {
			break;
		}

		if (buffer[i] == 0x00) {
			len = i;
			last = 1; /* one last pass through the for loop to sync the state machine */

		}
	}

	lua_pushlstring(L, buffer, len);
	return 1;
}

/* module table */
static const luaL_reg R[] = {
	{"setspeed",		nixio_spi_setspeed},
	{"read",		nixio_spi_read},
	{NULL,			NULL}
};

void nixio_open_spi(lua_State *L) {
	lua_newtable(L);
	luaL_register(L, NULL, R);
	lua_setfield(L, -2, "spi");
}
