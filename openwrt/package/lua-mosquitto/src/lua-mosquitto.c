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
	struct mosquitto *mosq;
	void *obj;
} ctx_t;

/* raises an error with mosquitto-specific strerr */
static int mosq__error(lua_State *L, int mosq_errno) {
    return luaL_error(L, mosquitto_strerror(mosq_errno));
}

/* pushes nil, mosquitto-specific error number and strerr on the stack */
static int mosq__perror(lua_State *L, int mosq_errno) {
	lua_pushnil(L);
	lua_pushinteger(L, mosq_errno);
	lua_pushstring(L, mosquitto_strerror(mosq_errno));
	return 3;
}

/* pushes nil, mosquitto-specific error number and strerr on the stack */
static int mosq__psyserror(lua_State *L) {
	lua_pushnil(L);
	lua_pushinteger(L, errno);
	lua_pushstring(L, strerror(errno));
	return 3;
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
	return 0;
}

static int mosq_cleanup(lua_State *L)
{
	mosquitto_lib_cleanup();
	return 0;
}

static int mosq_new(lua_State *L)
{
	const char *id = (lua_isnil(L, 1) ? NULL : luaL_checkstring(L, 1));
	bool clean_session = lua_toboolean(L, 2);

	if (id == NULL && !clean_session) {
		return luaL_argerror(L, 2, "if 'id' is nil then 'clean session' must be true");
	}

	ctx_t *ctx = (ctx_t *) lua_newuserdata(L, sizeof(ctx_t));

	ctx->obj = NULL;
	ctx->mosq = mosquitto_new(id, clean_session, ctx->obj);

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

	return 0;
}

static int ctx_connect(lua_State *L)
{
	/* TODO add sensible defaults, especially for port & keepalive */
	ctx_t *ctx = ctx_check(L);
	const char *host = luaL_checkstring(L, 2);
	int port = luaL_checkint(L, 3);
	int keepalive = luaL_checkint(L, 4);

	int rc = mosquitto_connect(ctx->mosq, host, port, keepalive);

	switch (rc) {
		case MOSQ_ERR_INVAL:
			return mosq__error(L, rc);
			break;

		case MOSQ_ERR_ERRNO:
			return mosq__psyserror(L);
			break;
	};

	lua_pushboolean(L, true);

	return 1;
}

static int ctx_reconnect(lua_State *L)
{
	ctx_t *ctx = ctx_check(L);

	int rc = mosquitto_reconnect(ctx->mosq);

	switch (rc) {
		case MOSQ_ERR_INVAL:
			return mosq__error(L, rc);
			break;

		case MOSQ_ERR_ERRNO:
			return mosq__psyserror(L);
			break;
	};

	lua_pushboolean(L, true);

	return 1;
}

static int ctx_disconnect(lua_State *L)
{
	ctx_t *ctx = ctx_check(L);

	int rc = mosquitto_disconnect(ctx->mosq);

	switch (rc) {
		case MOSQ_ERR_INVAL:
			return mosq__error(L, rc);
			break;

		case MOSQ_ERR_NO_CONN:
			return mosq__perror(L, rc);
			break;
	};

	lua_pushboolean(L, true);

	return 1;
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
	{"reconnect",		ctx_reconnect},
	{"disconnect",		ctx_disconnect},
	{NULL,		NULL}
};

int luaopen_mosquitto(lua_State *L)
{
	luaL_newmetatable(L, MOSQ_META_CTX);
	lua_pushvalue(L, -1);
	lua_setfield(L, -2, "__index");
	luaL_register(L, NULL, ctx_M);

	luaL_register(L, "mosquitto", R);

	return 1;
}
