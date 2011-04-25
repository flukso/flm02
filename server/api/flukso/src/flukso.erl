%% @author Bart Van Der Meerssche <bart.vandermeerssche@flukso.net>
%% @copyright (C) 2009-2011 Bart Van Der Meerssche
%%%
%%% This program is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%%
%% @doc Flukso module spec 

-module(flukso).
-author('Bart Van Der Meerssche <bart.vandermeerssche@flukso.net>').

-export([start/0, start_link/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok
    end.

mysql_prepare() ->
    mysql:prepare(watchdog, <<"INSERT INTO watchdog (uid, type, message, variables, severity, location, hostname, timestamp) VALUES (?, ?, ?, ?, ?, ?, ?, ?)">>),
    mysql:prepare(permissions, <<"SELECT permissions FROM logger_tokens WHERE meter = ? AND token = ?">>),
    mysql:prepare(sensor_key, <<"SELECT sha FROM (logger_devices ld INNER JOIN logger_meters lm ON ld.device = lm.device) WHERE lm.meter = ?">>),
    mysql:prepare(sensor_props, <<"SELECT uid, device, night FROM logger_meters WHERE meter = ?">>),
    mysql:prepare(sensor_update, <<"UPDATE logger_meters SET access = ?, night = ?, value = ? WHERE meter = ?">>),
    mysql:prepare(sensor_config, <<"UPDATE logger_meters SET class = ?, type = ?, function = ?, voltage = ?, current = ?, constant = ?, enabled = ? WHERE meter = ?">>),
    mysql:prepare(timezone, <<"SELECT timezone FROM users WHERE uid = ?">>),
    mysql:prepare(device_key, <<"SELECT sha FROM logger_devices WHERE device = ?">>),
    mysql:prepare(device_props, <<"SELECT sha, upgrade, resets FROM logger_devices WHERE device = ?">>),
    mysql:prepare(device_update, <<"UPDATE logger_devices SET access = ?, version = ?, upgrade = ?, resets = ?, uptime = ?, memtotal = ?, memfree = ?, memcached = ?, membuffers = ? WHERE device = ?">>).

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    flukso_deps:ensure(),
    ensure_started(crypto),
    ensure_started(erlrrd),
    ensure_started(mysql),
    mysql_prepare(),
    ensure_started(webmachine),
    flukso_sup:start_link().

%% @spec start() -> ok
%% @doc Start the flukso server.
start() ->
    flukso_deps:ensure(),
    ensure_started(crypto),
    ensure_started(erlrrd),
    ensure_started(mysql),
    mysql_prepare(),
    ensure_started(webmachine),
    application:start(flukso).

%% @spec stop() -> ok
%% @doc Stop the flukso server.
stop() -> 
    Res = application:stop(flukso),
    application:stop(webmachine),
    application:stop(mysql),
    application:stop(erlrrd),
    application:stop(crypto),
    Res.
