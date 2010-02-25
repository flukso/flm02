%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(flukso).
-author('author <author@example.com>').
-export([start/0, start_link/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok
    end.

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    flukso_deps:ensure(),
    ensure_started(erlrrd),
    ensure_started(crypto),
    ensure_started(webmachine),
    flukso_sup:start_link().

%% @spec start() -> ok
%% @doc Start the flukso server.
start() ->
    flukso_deps:ensure(),
    ensure_started(erlrrd),
    ensure_started(crypto),
    ensure_started(webmachine),
     application:start(flukso).

%% @spec stop() -> ok
%% @doc Stop the flukso server.
stop() -> 
    Res = application:stop(flukso),
    application:stop(erlrrd),
    application:stop(webmachine),
    application:stop(crypto),
    Res.

