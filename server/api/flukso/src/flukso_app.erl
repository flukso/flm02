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
%% @doc Callbacks for the flukso application.

-module(flukso_app).
-author('Bart Van Der Meerssche <bart.vandermeerssche@flukso.net>').

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
