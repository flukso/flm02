%% @author icarus75 <bart.vandermeerssche@flukso.net>
%% @copyright 2009-2010 flukso.net
%% @doc Flukso webmachine_resource.

-module(flukso_resource).
-export([init/1, allowed_methods/2, malformed_request/2, is_authorized/2, content_types_provided/2, to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").

-record(state,
        {rrdSensor,
         rrdStart,
         rrdEnd,
         rrdResolution,
         rrdFactor,
         token,
         jsonpCallback}).

init([]) -> 
    {ok, undefined}.

allowed_methods(ReqData, State) ->
    {['GET'], ReqData, State}.

malformed_request(ReqData, _State) ->
    {_Version, ValidVersion} = check_version(wrq:get_req_header("X-Version", ReqData), wrq:get_qs_value("version", ReqData)),
    {RrdSensor, ValidSensor} = check_sensor(wrq:path_info(sensor, ReqData)),
    {RrdStart, RrdEnd, RrdResolution, ValidTime} = check_time(wrq:get_qs_value("interval", ReqData), wrq:get_qs_value("start", ReqData), wrq:get_qs_value("end", ReqData), wrq:get_qs_value("resolution", ReqData)),
    {RrdFactor, ValidUnit} = check_unit(wrq:get_qs_value("unit", ReqData)),
    {Token, ValidToken} = check_token(wrq:get_req_header("X-Token", ReqData), wrq:get_qs_value("token", ReqData)),
    {JsonpCallback, ValidJsonpCallback} = check_jsonp_callback(wrq:get_qs_value("jsonp_callback", ReqData)),

    State = #state{rrdSensor = RrdSensor, 
                   rrdStart = RrdStart,
                   rrdEnd = RrdEnd,
                   rrdResolution = RrdResolution,
                   rrdFactor = RrdFactor,
                   token = Token,
                   jsonpCallback = JsonpCallback},

    {case {ValidVersion, ValidSensor, ValidTime, ValidUnit, ValidToken, ValidJsonpCallback}  of
	{true, true, true, true, true, true} -> false;
	_ -> true
     end, 
    ReqData, State}.

is_authorized(ReqData, #state{rrdSensor = RrdSensor, token = Token} = State) ->
    {data, Result} = mysql:execute(pool, permissions, [RrdSensor, Token]),

    {case mysql:get_result_rows(Result) of
        [[62]] -> true;
        _Permission -> "access refused" 
    end,
    ReqData, State}.

content_types_provided(ReqData, State) -> 
    {[{"application/json", to_json}], ReqData, State}.

to_json(ReqData, #state{rrdSensor = RrdSensor, rrdStart = RrdStart, rrdEnd = RrdEnd, rrdResolution = RrdResolution, rrdFactor = RrdFactor, jsonpCallback = JsonpCallback} = State) -> 
    case wrq:get_qs_value("interval", ReqData) of
        "night"   -> Path = "var/data/night/";
        _Interval -> Path = "var/data/base/"
    end,

%% debugging: io:format("~s~n", [erlrrd:c([[Path, [RrdSensor|".rrd"]], "AVERAGE", ["-s ", RrdStart], ["-e ", RrdEnd], ["-r ", RrdResolution]])]),

    case erlrrd:fetch(erlrrd:c([[Path, [RrdSensor|".rrd"]], "AVERAGE", ["-s ", RrdStart], ["-e ", RrdEnd], ["-r ", RrdResolution]])) of
        {ok, Response} ->
            Filtered = [re:split(X, "[:][ ]", [{return,list}]) || [X] <- Response, string:str(X, ":") == 11],
            Datapoints = [[list_to_integer(X), round(list_to_float(Y) * RrdFactor)] || [X, Y] <- Filtered, string:len(Y) /= 3],
            Nans = [[list_to_integer(X), list_to_binary(Y)] || [X, Y] <- Filtered, string:len(Y) == 3],
            Final = mochijson2:encode(lists:merge(Datapoints, Nans)),
            {case JsonpCallback of
                undefined -> Final;
                _ -> [JsonpCallback, "(", Final, ");"]
             end,
            ReqData, State};

        {error, _Reason} ->
            {{halt, 404}, ReqData, State}
    end.

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
    check_time(Interval, undefined, undefined, "");
check_time(Interval, undefined, undefined, Resolution) ->
    Now = unix_time(),

    Intervals = [{"hour", "end-1h", 60},
                 {"day", "end-1d", 900},
                 {"month", "end-30d", 86400},
                 {"year", "end-1y", 604800},
                 {"night", "end-30d", 86400}],

    case {lists:keyfind(Interval, 1, Intervals), re:run(Resolution, "[0-9]+", [])} of
        {false, _} -> {false, false, false, false};
        {{_Interval, Start, _DefResolution}, {match, [{0,_}]}} ->
            AlignedEnd = integer_to_list(time_align(Now, list_to_integer(Resolution))),
            {Start, AlignedEnd, Resolution, true};
        {{_Interval, Start, DefResolution}, _} ->
            AlignedEnd = integer_to_list(time_align(Now, DefResolution)),
            {Start, AlignedEnd, integer_to_list(DefResolution), true}
    end;
check_time(undefined, Start, undefined, Resolution) ->
    check_time(undefined, Start, integer_to_list(unix_time()), Resolution);
check_time(undefined, Start, End, undefined) ->
    check_time(undefined, Start, End, "60");
check_time(undefined, Start, End, Resolution) ->
    case {re:run(Start, "[0-9]+", []), re:run(End, "[0-9]+", []), re:run(Resolution, "[0-9]+", [])} of
        {{match, [{0,_}]}, {match, [{0,_}]}, {match, [{0,_}]}} ->
            AlignedStart = integer_to_list(time_align(list_to_integer(Start), list_to_integer(Resolution))),
            AlignedEnd = integer_to_list(time_align(list_to_integer(End), list_to_integer(Resolution))),
            {AlignedStart, AlignedEnd, Resolution, true};
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
