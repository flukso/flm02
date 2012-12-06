/*

  lua-mosquitto.c - libmosquitto bindings for Lua

  Copyright (C) 2012 Bart Van Der Meerssche <bart.vandermeerssche@flukso.net>

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.

*/

#include <stdio.h>
#include <string.h>
#include <errno.h>

#include <lua.h>
#include <lualib.h>
#include <lauxlib.h>

#include <mosquitto.h>

/* unique naming for userdata metatables */
#define MOSQ_META_CTX	"mosquitto.ctx"

typedef struct {
	lua_State *L;
	struct mosquitto *mosq;
	int on_message;
} ctx_t;

/* handle mosquitto lib return codes */
static int mosq__pstatus(lua_State *L, int mosq_errno) {
	switch (mosq_errno) {
		case MOSQ_ERR_SUCCESS:
			lua_pushboolean(L, true);
			return 1;
			break;

		case MOSQ_ERR_INVAL:
		case MOSQ_ERR_NOMEM:
		case MOSQ_ERR_PROTOCOL:
		case MOSQ_ERR_NOT_SUPPORTED:
			return luaL_error(L, mosquitto_strerror(mosq_errno));
			break;

		case MOSQ_ERR_NO_CONN:
		case MOSQ_ERR_CONN_LOST:
		case MOSQ_ERR_PAYLOAD_SIZE:
			lua_pushnil(L);
			lua_pushinteger(L, mosq_errno);
			lua_pushstring(L, mosquitto_strerror(mosq_errno));
			return 3;
			break;

		case MOSQ_ERR_ERRNO:
			lua_pushnil(L);
			lua_pushinteger(L, errno);
			lua_pushstring(L, strerror(errno));
			return 3;
			break;
	};

	return 0;
}

static int mosq_version(lua_State *L)
{
	int major, minor, rev;
	char version[16];

	mosquitto_lib_version(&major, &minor, &rev);
	sprintf(version, "%i.%i.%i", major, minor, rev);
	lua_pushstring(L, version);
	return 1;
}

static int mosq_init(lua_State *L)
{
	mosquitto_lib_init();
	return mosq__pstatus(L, MOSQ_ERR_SUCCESS);
}

static int mosq_cleanup(lua_State *L)
{
	mosquitto_lib_cleanup();
	return mosq__pstatus(L, MOSQ_ERR_SUCCESS);
}

static int mosq_new(lua_State *L)
{
	const char *id = (lua_isnil(L, 1) ? NULL : luaL_checkstring(L, 1));
	bool clean_session = lua_toboolean(L, 2);

	if (id == NULL && !clean_session) {
		return luaL_argerror(L, 2, "if 'id' is nil then 'clean session' must be true");
	}

	ctx_t *ctx = (ctx_t *) lua_newuserdata(L, sizeof(ctx_t));

	/* ctx will be passed as void *obj arg in the callback functions */
	ctx->L = L;
	ctx->mosq = mosquitto_new(id, clean_session, ctx);

	if (ctx->mosq == NULL) {
		return luaL_error(L, strerror(errno));
	}

	luaL_getmetatable(L, MOSQ_META_CTX);
	lua_setmetatable(L, -2);

	return 1;
}

static ctx_t * ctx_check(lua_State *L)
{
	return (ctx_t *) luaL_checkudata(L, 1, MOSQ_META_CTX);
}

static int ctx_destroy(lua_State *L)
{
	ctx_t *ctx = ctx_check(L);
	mosquitto_destroy(ctx->mosq);

	return mosq__pstatus(L, MOSQ_ERR_SUCCESS);
}

static int ctx_connect(lua_State *L)
{
	/* TODO add sensible defaults, especially for port & keepalive */
	ctx_t *ctx = ctx_check(L);
	const char *host = luaL_checkstring(L, 2);
	int port = luaL_checkint(L, 3);
	int keepalive = luaL_checkint(L, 4);

	int rc =  mosquitto_connect(ctx->mosq, host, port, keepalive);
	return mosq__pstatus(L, rc);
}

static int ctx_connect_async(lua_State *L)
{
	/* TODO add sensible defaults, especially for port & keepalive */
	ctx_t *ctx = ctx_check(L);
	const char *host = luaL_checkstring(L, 2);
	int port = luaL_checkint(L, 3);
	int keepalive = luaL_checkint(L, 4);

	int rc =  mosquitto_connect_async(ctx->mosq, host, port, keepalive);
	return mosq__pstatus(L, rc);
}

static int ctx_reconnect(lua_State *L)
{
	ctx_t *ctx = ctx_check(L);

	int rc = mosquitto_reconnect(ctx->mosq);
	return mosq__pstatus(L, rc);
}

static int ctx_disconnect(lua_State *L)
{
	ctx_t *ctx = ctx_check(L);

	int rc = mosquitto_disconnect(ctx->mosq);
	return mosq__pstatus(L, rc);
}

static int ctx_publish(lua_State *L)
{
	ctx_t *ctx = ctx_check(L);
	int mid;	/* message id is referenced in the publish callback */
	const char *topic = luaL_checkstring(L, 2);

	if (!lua_isstring(L, 3)) {
		return luaL_argerror(L, 3, "payload should be a string or number");
	};

	size_t payloadlen;
	const void *payload = (const void *) lua_tolstring(L, 3, &payloadlen);

	int qos = luaL_checkint(L, 4);
	bool retain = lua_toboolean(L, 5);

	int rc = mosquitto_publish(ctx->mosq, &mid, topic, payloadlen, payload, qos, retain);

	if (rc != MOSQ_ERR_SUCCESS) {
		return mosq__pstatus(L, rc);
	} else {
		lua_pushinteger(L, mid);
		return 1;
	};
}

static int ctx_subscribe(lua_State *L)
{
	ctx_t *ctx = ctx_check(L);
	int mid;
	const char *sub = luaL_checkstring(L, 2);
	int qos = luaL_checkinteger(L, 3);

	int rc = mosquitto_subscribe(ctx->mosq, &mid, sub, qos);

	if (rc != MOSQ_ERR_SUCCESS) {
		return mosq__pstatus(L, rc);
	} else {
		lua_pushinteger(L, mid);
		return 1;
	};
}

static int ctx_loop(lua_State *L)
{
	ctx_t *ctx = ctx_check(L);
	int timeout = luaL_checkint(L, 2);
	int max_packets = luaL_checkint(L, 3);

	int rc = mosquitto_loop(ctx->mosq, timeout, max_packets);
	return mosq__pstatus(L, rc);
}

static int ctx_loop_start(lua_State *L)
{
	ctx_t *ctx = ctx_check(L);

	int rc = mosquitto_loop_start(ctx->mosq);
	return mosq__pstatus(L, rc);
}

static int ctx_socket(lua_State *L)
{
	ctx_t *ctx = ctx_check(L);

	int fd = mosquitto_socket(ctx->mosq);
	switch (fd) {
		case -1:
			lua_pushboolean(L, false);
			break;
		default:
			lua_pushinteger(L, fd);
			break;
	}

	return 1;
}

static int ctx_loop_read(lua_State *L)
{
	ctx_t *ctx = ctx_check(L);
	int max_packets = luaL_checkint(L, 2);

	int rc = mosquitto_loop_read(ctx->mosq, max_packets);
	return mosq__pstatus(L, rc);
}

static int ctx_loop_write(lua_State *L)
{
	ctx_t *ctx = ctx_check(L);
	int max_packets = luaL_checkint(L, 2);

	int rc = mosquitto_loop_write(ctx->mosq, max_packets);
	return mosq__pstatus(L, rc);
}

static int ctx_loop_misc(lua_State *L)
{
	ctx_t *ctx = ctx_check(L);

	int rc = mosquitto_loop_misc(ctx->mosq);
	return mosq__pstatus(L, rc);
}

static int ctx_want_write(lua_State *L)
{
	ctx_t *ctx = ctx_check(L);

	lua_pushboolean(L, mosquitto_want_write(ctx->mosq));
	return 1;
}

static void ctx_on_message(
	struct mosquitto *mosq,
	void *obj,
	const struct mosquitto_message *msg)
{
	ctx_t *ctx = obj;

	/* push registered Lua callback function onto the stack */
	lua_rawgeti(ctx->L, LUA_REGISTRYINDEX, ctx->on_message);
	/* push function args */
	lua_pushinteger(ctx->L, msg->mid);
	lua_pushstring(ctx->L, msg->topic);
	lua_pushlstring(ctx->L, msg->payload, msg->payloadlen);
	lua_pushinteger(ctx->L, msg->qos);
	lua_pushboolean(ctx->L, msg->retain);

	lua_pcall(ctx->L, 5, 0, 0); /* args: mid, topic, payload, qos, retain */
}

static int ctx_message_callback_set(lua_State *L)
{
	ctx_t *ctx = ctx_check(L);

	if (!lua_isfunction(L, 2)) {
		return luaL_argerror(L, 2, "expecting a callback function");
	}

	/* pop the function from the stack and store it into the registry */
	int ref = luaL_ref(L, LUA_REGISTRYINDEX);
	/* store the reference into the context, to be retrieved by ctx_on_message */
	ctx->on_message = ref;
	/* register C callback in mosquitto */
	mosquitto_message_callback_set(ctx->mosq, ctx_on_message);

	return mosq__pstatus(L, MOSQ_ERR_SUCCESS);
}

static const struct luaL_Reg R[] = {
	{"version",	mosq_version},
	{"init",	mosq_init},
	{"cleanup",	mosq_cleanup},
	{"__gc",	mosq_cleanup},
	{"new",		mosq_new},
	{NULL,		NULL}
};

static const struct luaL_Reg ctx_M[] = {
	{"destroy",			ctx_destroy},
	{"__gc",			ctx_destroy},
/*	{"reinitialise", 	ctx_reinitialise},
	{"set_will", 		ctx_will_set},
	{"clear_will",		ctx_will_clear},
	{"set_login",		ctx_login_set},			TODO */
	{"connect",			ctx_connect},
	{"connect_async",	ctx_connect_async},
	{"reconnect",		ctx_reconnect},
	{"disconnect",		ctx_disconnect},
	{"publish",			ctx_publish},
	{"subscribe",		ctx_subscribe},
/*	{"unsubscribe",		ctx_unsubscribe},		TODO */
	{"loop",			ctx_loop},
	{"start_loop",		ctx_loop_start},
/*	{"stop_loop",		ctx_loop_stop},			TODO */
	{"socket",			ctx_socket},
	{"read",			ctx_loop_read},
	{"write",			ctx_loop_write},
	{"misc",			ctx_loop_misc},
	{"want_write",		ctx_want_write},
	{"message_callback",	ctx_message_callback_set},
	{NULL,		NULL}
};

int luaopen_mosquitto(lua_State *L)
{
	/* set private environment for this module */
	lua_newtable(L);
	lua_replace(L, LUA_ENVIRONINDEX);

	/* metatable.__index = metatable */
	luaL_newmetatable(L, MOSQ_META_CTX);
	lua_pushvalue(L, -1);
	lua_setfield(L, -2, "__index");
	luaL_register(L, NULL, ctx_M);

	luaL_register(L, "mosquitto", R);

	return 1;
}
