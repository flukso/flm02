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
%% @doc Common record definitions and helper functions for the Flukso API. 

-record(state,
        {rrdSensor,
         rrdStart,
         rrdEnd,
         rrdResolution,
         rrdFactor,
         token,
         jsonpCallback}).

%% checks
check_version(undefined, undefined) ->
    {false, false};
check_version(Version, undefined) ->
    case Version of
        "1.0" -> {Version, true};
        _ -> {false, false}
    end;
check_version(undefined, Version) ->
    check_version(Version, undefined);
check_version(_, _) ->
    {false, false}.

check_sensor(Sensor) ->
    case re:run(Sensor, "[0-9a-f]+", []) of 
        {match, [{0,32}]} -> {Sensor, true};
        _ -> {false, false}
    end.

check_time(undefined, undefined, _End, _Resolution) ->
    {false, false, false, false};
check_time(Interval, undefined, undefined, undefined) ->
    case default_resolution(Interval) of
        false -> {false, false, false, false};
        DefResolution -> check_time(Interval, undefined, undefined, DefResolution)
    end;
check_time(Interval, undefined, undefined, Resolution) ->
    Now = unix_time(),
    case {time_to_seconds(Interval), time_to_seconds(Resolution)} of
        {false, _} -> {false, false, false, false};
        {_, false} -> {false, false, false, false};
        {IntervalSec, ResolutionSec} -> 
            AlignedEnd = time_align(Now, ResolutionSec),
            AlignedStart = AlignedEnd - IntervalSec,
            {integer_to_list(AlignedStart), integer_to_list(AlignedEnd), integer_to_list(ResolutionSec), true}
    end;
check_time(undefined, Start, undefined, Resolution) ->
    check_time(undefined, Start, integer_to_list(unix_time()), Resolution);
check_time(undefined, Start, End, undefined) ->
    check_time(undefined, Start, End, "minute");
check_time(undefined, Start, End, Resolution) ->
    case {re:run(Start, "[0-9]+", []), re:run(End, "[0-9]+", []), time_to_seconds(Resolution)} of
        {_, _, false} -> {false, false, false, false};
        {{match, [{0,_}]}, {match, [{0,_}]}, ResolutionSec} ->
            AlignedStart = time_align(list_to_integer(Start), ResolutionSec),
            AlignedEnd = time_align(list_to_integer(End), ResolutionSec),
            {integer_to_list(AlignedStart), integer_to_list(AlignedEnd), integer_to_list(ResolutionSec), true};
        _ -> {false, false, false, false}
    end;
check_time(_, _, _, _) ->
    {false, false, false, false}.

check_unit(Unit) ->
    Units = [{"watt", 3600},
             {"kwhperyear", 31536},
             {"eurperyear", 5676},
             {"audperyear", 5991}],

    case lists:keyfind(Unit, 1, Units) of
        false -> {false, false};
        {_Unit, RrdFactor} -> {RrdFactor, true}
    end.

check_token(undefined, undefined) ->
    {false, false};
check_token(Token, undefined) ->
    check_sensor(Token);
check_token(undefined, Token) ->
    check_sensor(Token);
check_token(_, _) ->
    {false, false}.

check_jsonp_callback(undefined) ->
    {undefined, true};
check_jsonp_callback(JsonpCallback) ->
    Length = string:len(JsonpCallback),

    case re:run(JsonpCallback, "[0-9a-zA-Z_]+", []) of
        {match, [{0, Length}]} -> {JsonpCallback, true};
        _ -> {false, false}
    end.

%% helper functions
unix_time() ->
    {Megaseconds, Seconds, _Microseconds} = erlang:now(),
    Megaseconds*1000000 + Seconds.

time_align(Time, Resolution) ->
    (Time div Resolution) * Resolution.

default_resolution(Interval) ->
    DefResolutions = [{"15min", "minute"},
                      {"hour", "minute"},
                      {"day", "15min"},
                      {"week", "day"},
                      {"month", "day"},
                      {"year", "week"},
                      {"night", "day"}],

    case lists:keyfind(Interval, 1, DefResolutions) of
        false -> false;
        {_Interval, Defresolution} -> Defresolution
    end.

time_to_seconds(Time) ->
    Times = [{"minute", 60},
             {"15min", 900},
             {"hour", 3600},
             {"day", 86400},
             {"week", 604800},
             {"month", 2419200},
             {"year", 31536000},
             {"night", 2419200}],

    case lists:keyfind(Time, 1, Times) of
        false -> false;
        {_Time, TimeSec} -> TimeSec
    end.
