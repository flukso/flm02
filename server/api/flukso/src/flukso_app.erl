%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the flukso application.

-module(flukso_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for flukso.
start(_Type, _StartArgs) ->
    flukso_deps:ensure(),
    flukso_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for flukso.
stop(_State) ->
    ok.
