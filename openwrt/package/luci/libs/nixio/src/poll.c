/*
 * nixio - Linux I/O library for lua
 *
 *   Copyright (C) 2009 Steven Barth <steven@midlink.org>
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
#include <time.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>
#include <sys/time.h>


static int nixio_gettimeofday(lua_State *L) {
	struct timeval tv;
	gettimeofday(&tv, NULL);
	nixio__pushnumber(L, tv.tv_sec);
	nixio__pushnumber(L, tv.tv_usec);
	return 2;
}

static int nixio_settimeofday(lua_State *L) {
	struct timeval tv;
	tv.tv_sec = luaL_optint(L, 1, 0);
	tv.tv_usec = luaL_optint(L, 2, 0);

	int status = settimeofday(&tv, NULL);

	if (!status) {
		lua_pushboolean(L, 1);
		return 1;
	} else {
		return nixio__perror(L);
	}
}

/**
 * nanosleep()
 */
static int nixio_nanosleep(lua_State *L) {
	struct timespec req, rem;
	req.tv_sec = luaL_optint(L, 1, 0);
	req.tv_nsec = luaL_optint(L, 2, 0);

	int status = nanosleep(&req, &rem);
	if (!status) {
		lua_pushboolean(L, 1);
		return 1;
	} else {
		if (errno == EINTR) {
			lua_pushboolean(L, 0);
			lua_pushinteger(L, rem.tv_sec);
			lua_pushinteger(L, rem.tv_nsec);
			return 3;
		} else {
			return nixio__perror(L);
		}
	}
}

/**
 * Checks whether a flag is set in the bitmap and sets the matching table value
 */
static void nixio_poll_flags__r(lua_State *L, int *map, int f, const char *t) {
	lua_pushstring(L, t);
	if (*map & f) {
		lua_pushboolean(L, 1);
	} else {
		lua_pushnil(L);
	}
	lua_rawset(L, -3);
}

/**
 * Translate integer to poll flags and vice versa
 */
static int nixio_poll_flags(lua_State *L) {
	int flags;
	if (lua_isnumber(L, 1)) {
		flags = luaL_checkinteger(L, 1);
		lua_newtable(L);
		nixio_poll_flags__r(L, &flags, POLLIN, "in");
		nixio_poll_flags__r(L, &flags, POLLOUT, "out");
		nixio_poll_flags__r(L, &flags, POLLERR, "err");
#ifndef __WINNT__
		nixio_poll_flags__r(L, &flags, POLLPRI, "pri");
		nixio_poll_flags__r(L, &flags, POLLHUP, "hup");
		nixio_poll_flags__r(L, &flags, POLLNVAL, "nval");
#endif
	 } else {
		flags = 0;
		const int j = lua_gettop(L);
		for (int i=1; i<=j; i++) {
			const char *flag = luaL_checkstring(L, i);
			if (!strcmp(flag, "in")) {
				flags |= POLLIN;
			} else if (!strcmp(flag, "out")) {
				flags |= POLLOUT;
			} else if (!strcmp(flag, "err")) {
				flags |= POLLERR;
			} else if (!strcmp(flag, "pri")) {
#ifndef __WINNT__
				flags |= POLLPRI;
#endif
			} else if (!strcmp(flag, "hup")) {
#ifndef __WINNT__
				flags |= POLLHUP;
#endif
			} else if (!strcmp(flag, "nval")) {
#ifndef __WINNT__
				flags |= POLLNVAL;
#endif
			} else {
				return luaL_argerror(L, i,
				 "supported values: in, pri, out, err, hup, nval");
			}
		}
		lua_pushinteger(L, flags);
	}
	return 1;
}

/**
 * poll({{fd = socket, events = FLAGS}, ...}, timeout)
 */
static int nixio_poll(lua_State *L) {
	int len = lua_objlen(L, 1);
	int i, fd;
	int timeout = luaL_optint(L, 2, 0);
	int status = -1;

	/* we are being abused as sleep() replacement... */
	if (lua_isnoneornil(L, 1) || len < 1) {
		if (!poll(NULL, 0, timeout)) {
			lua_pushinteger(L, 0);
			return 1;
		} else {
			return nixio__perror(L);
		}
	}

	luaL_checktype(L, 1, LUA_TTABLE);
	struct pollfd *fds = calloc(len, sizeof(struct pollfd));
	if (!fds) {
		return luaL_error(L, NIXIO_OOM);
	}

	for (i = 0; i < len; i++) {
		lua_rawgeti(L, 1, i+1);
		if (!lua_istable(L, -1)) {
			free(fds);
			return luaL_argerror(L, 1, "invalid datastructure");
		}

		lua_pushliteral(L, "fd");
		lua_rawget(L, -2);
		fd = nixio__tofd(L, -1);
		if (fd == -1) {
			free(fds);
			return luaL_argerror(L, 1, "invalid fd in datastructure");
		}
		fds[i].fd = fd;

		lua_pushliteral(L, "events");
		lua_rawget(L, -3);
		fds[i].events = (short)lua_tointeger(L, -1);

		lua_pop(L, 3);
	}

	status = poll(fds, (nfds_t)len, timeout);

	if (status == 0) {
		free(fds);
		lua_pushinteger(L, status);
		return 1;
	} else if (status < 0) {
		free(fds);
		return nixio__perror(L);
	}

	for (i = 0; i < len; i++) {
		lua_rawgeti(L, 1, i+1);

		lua_pushliteral(L, "revents");
		lua_pushinteger(L, fds[i].revents);
		lua_rawset(L, -3);

		lua_pop(L, 1);
	}

	free(fds);

	lua_pushinteger(L, status);
	lua_pushvalue(L, 1);

	return 2;
}

#ifdef __linux__

#include <stdint.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/timerfd.h>

static int nixio_timerfd(lua_State *L) {
	struct itimerspec its;

	its.it_value.tv_sec = (time_t)luaL_optinteger(L, 1, 0);
	its.it_value.tv_nsec = (long)luaL_optinteger(L, 2, 0);

	its.it_interval.tv_sec = (time_t)luaL_optinteger(L, 3, 0);
	its.it_interval.tv_nsec = (long)luaL_optinteger(L, 4, 0);

	/* Create a timer object and associated fd */
	int fd = timerfd_create(CLOCK_REALTIME, 0);

	if (fd == -1) {
		return nixio__perror(L);
	}

	/* Workaround for TFD_NONBLOCK 'invalid argument' in uClibc*/
	int flags;
	flags = fcntl(fd, F_GETFL);
	if (flags == -1)
		return nixio__perror(L);
	flags |= O_NONBLOCK;
	if(fcntl(fd, F_SETFL, flags) == -1)
		return nixio__perror(L);

	/* Arm the timer */
	if (timerfd_settime(fd, 0, &its ,NULL) == -1) {
		close(fd);
		return nixio__perror(L);
	}

	/* Create a userdatum for fd */
	int *udata = lua_newuserdata(L, sizeof(int));
	if (!udata) {
		close(fd);
		return luaL_error(L, "out of memory");
	}

	*udata = fd;

	luaL_getmetatable(L, NIXIO_FILE_META);
	lua_setmetatable(L, -2);

	return 1;
}

#include <signal.h>
#include <sys/signalfd.h>

static int nixio_setitimerfd(lua_State *L) {
	struct itimerval itv;

	itv.it_value.tv_sec = (time_t)luaL_optinteger(L, 1, 0);
	itv.it_value.tv_usec = (suseconds_t)luaL_optinteger(L, 2, 0);

	itv.it_interval.tv_sec = (time_t)luaL_optinteger(L, 3, 0);
	itv.it_interval.tv_usec = (suseconds_t)luaL_optinteger(L, 4, 0);

	sigset_t sigmask, sigmask_old;

	/* Initialize a signal mask containing SIGALRM */
	sigemptyset(&sigmask);
	sigaddset(&sigmask, SIGALRM);

	/* Block SIGALRM */
	if(sigprocmask(SIG_BLOCK, &sigmask, &sigmask_old) == -1) {
		return nixio__perror(L);
	}

	/* Signals in sigmask are delivered synchronously though an fd */
	int fd = signalfd(-1, &sigmask, 0);

	if(fd == -1) {
		sigprocmask(SIG_BLOCK, &sigmask_old, NULL);
		return nixio__perror(L);
	}

	/* Set the signal fd non-blocking */
	int flags;
	flags = fcntl(fd, F_GETFL);
	if (flags == -1)
		return nixio__perror(L);
	flags |= O_NONBLOCK;
	if(fcntl(fd, F_SETFL, flags) == -1)
		return nixio__perror(L);

	/* Enable the (interval) timer */	
	if (setitimer(ITIMER_REAL, &itv, NULL) == -1) {
		close(fd);
		sigprocmask(SIG_BLOCK, &sigmask_old, NULL);
		return nixio__perror(L);
	}

	/* Create a userdatum for fd */
	int *udata = lua_newuserdata(L, sizeof(int));
	if (!udata) {
		return luaL_error(L, "out of memory");
	}

	*udata = fd;

	luaL_getmetatable(L, NIXIO_FILE_META);
	lua_setmetatable(L, -2);

	return 1;
}

#endif

/* module table */
static const luaL_reg R[] = {
#ifdef __linux__
	{"timerfd",	nixio_timerfd},
	{"setitimerfd",	nixio_setitimerfd},
#endif
	{"gettimeofday", nixio_gettimeofday},
	{"settimeofday", nixio_settimeofday},
	{"nanosleep",	nixio_nanosleep},
	{"poll",		nixio_poll},
	{"poll_flags",	nixio_poll_flags},
	{NULL,			NULL}
};

void nixio_open_poll(lua_State *L) {
	luaL_register(L, NULL, R);
}
