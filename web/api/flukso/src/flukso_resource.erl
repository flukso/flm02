%% @author icarus75 <bart.vandermeerssche@flukso.net>
%% @copyright 2009-2010 flukso.net
%% @doc Flukso webmachine_resource.

-module(flukso_resource).
-export([init/1, allowed_methods/2, malformed_request/2, content_types_provided/2, to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").

-record(state,
        {rrdSensor,
         rrdTime,
         rrdFactor}).

init([]) -> 
    {ok, undefined}.

allowed_methods(ReqData, State) ->
    {['GET'], ReqData, State}.

malformed_request(ReqData, _) ->
    {RrdSensor, ValidSensor} = rrd_sensor(wrq:path_info(sensor, ReqData)),
    {RrdTime, ValidInterval} = rrd_time(wrq:get_qs_value("interval", ReqData)),
    {RrdFactor, ValidUnit} = rrd_factor(wrq:get_qs_value("unit", ReqData)),

    State = #state{rrdSensor = RrdSensor, rrdTime = RrdTime, rrdFactor = RrdFactor},

    {case {ValidSensor, ValidInterval, ValidUnit}  of
	{true, true, true} -> false;
	_ -> true
     end, 
    ReqData, State}.

content_types_provided(ReqData, State) -> 
    {[{"application/json", to_json}], ReqData, State}.

to_json(ReqData, #state{rrdSensor = RrdSensor, rrdTime = RrdTime, rrdFactor = RrdFactor} = State) -> 
    case wrq:path_info(interval, ReqData) of
        "night"   -> Path = "var/data/night/";
        _Interval -> Path = "var/data/base/"
    end,

    case erlrrd:fetch(erlrrd:c([[Path, [RrdSensor|".rrd"]], "AVERAGE", ["-s",RrdTime]])) of
        {ok, Response} ->
            Filtered = [re:split(X, "[:][ ]", [{return,list}]) || [X] <- Response, string:str(X, ":") == 11],
            Datapoints = [[list_to_integer(X), round(list_to_float(Y) * RrdFactor)] || [X, Y] <- Filtered, string:len(Y) /= 3],
            Nans = [[list_to_integer(X), list_to_binary(Y)] || [X, Y] <- Filtered, string:len(Y) == 3],
            Final = lists:merge(Datapoints, Nans),
            {mochijson2:encode(Final), ReqData, State};

        {error, Reason} ->
            {{halt, 404}, ReqData, State}
    end.

rrd_sensor(Sensor) ->
    case re:run(Sensor, "[0-9a-f]+", []) of 
        {match, [{0,32}]} -> {Sensor, true};
        _ -> {false, false}
    end.

rrd_time(Interval) ->
    Intervals = [{"hour", "end-1h"},
                 {"day", "end-1d"},
                 {"month", "end-30d"},
                 {"year", "end-1y"},
                 {"night", "end-30d"}],

    case lists:keyfind(Interval, 1, Intervals) of
        false -> {false, false};
        {_Interval, RrdTime} -> {RrdTime, true}
    end.

rrd_factor(Unit) ->
    Units = [{"watt", 3600},
             {"kwhperyear", 31536},
             {"eurperyear", 5676},
             {"audperyear", 5991}],

    case lists:keyfind(Unit, 1, Units) of
        false -> {false, false};
        {_Unit, RrdFactor} -> {RrdFactor, true}
    end.

