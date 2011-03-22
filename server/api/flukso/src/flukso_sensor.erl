%% @author icarus75 <bart.vandermeerssche@flukso.net>
%% @copyright 2009-2010 flukso.net
%% @doc Flukso webmachine_resource.

-module(flukso_sensor).
-export([init/1,
         allowed_methods/2,
         malformed_request/2,
         is_authorized/2,
         process_post/2]).

-include_lib("webmachine/include/webmachine.hrl").

-record(state,
        {rrdSensor}).

init([]) -> 
    {ok, undefined}.

% debugging
%init(Config) ->
%   {{trace, "/tmp"}, Config}.

allowed_methods(ReqData, State) ->
    {['POST'], ReqData, State}.

malformed_request(ReqData, State) ->
    {_Version, ValidVersion} = check_version(wrq:get_req_header("X-Version", ReqData), wrq:get_qs_value("version", ReqData)),
% TODO: check validity of X-Device and X-Digest headers

    {case {ValidVersion} of
        {true} -> false;
        _ -> true
     end,
    ReqData, State}.

is_authorized(ReqData, State) ->
    {data, Result} = mysql:execute(pool, device_key, [wrq:get_req_header("X-Device", ReqData)]),
    [[Key]] = mysql:get_result_rows(Result),
    Data = wrq:req_body(ReqData),
    <<X:160/big-unsigned-integer>> = crypto:sha_mac(Key, Data),
    Digest = list_to_binary(io_lib:format("~40.16.0b", [X])),

    {case list_to_binary(wrq:get_req_header("X-Digest", ReqData)) of
        Digest -> true;
        _WrongDigest -> "access refused"
     end,
     ReqData, State}.

% JSON: {"measurements":{"<sensor1>":[[<ts11>,<value11>],...,[<ts1m>,<value1m>]],
%                        ...,
%                        "<sensorn>":[[<tsn1>,<valuen1>],...,[<tsnm>,<valuenm>]]}}
%
% Mochijson2: {struct,[{<<"measurements">>, {struct, [{<<"<sensor1>">>, [[<ts11>,<value11>],...,[<ts1m>,<value1m>]]},
%                                                     ...,
%                                                     {<<"<sensorn>">>, [[<tsn1>,<valuen1>],...,[<tsnm>,<valuenm>]]}]}}]}
%
process_post(ReqData, State) ->
    {struct, JsonData} = mochijson2:decode(wrq:req_body(ReqData)),
    {struct, Measurements} = proplists:get_value(<<"measurements">>, JsonData),
    Ids = proplists:get_keys(Measurements),
    RrdResponse = [update(RrdSensor, proplists:get_value(RrdSensor, Measurements)) || RrdSensor <- Ids],

    JsonResponse = mochijson2:encode({struct, [{<<"response">>, {struct, RrdResponse}}]}),
    {true , wrq:set_resp_body(JsonResponse, ReqData), State}.

update(RrdSensor, TimeSeries) ->
    Path = "var/data/base/",
    RrdData = [[integer_to_list(Time), ":", integer_to_list(Counter), " "] || [Time, Counter] <- TimeSeries],

%debugging: io:format("~s~n", [[Path, [binary_to_list(RrdSensor)|".rrd"], " ", RrdData]]),
 
    case erlrrd:update([Path, [binary_to_list(RrdSensor)|".rrd"], " ", RrdData]) of
        {ok, _RrdResponse} -> {RrdSensor, <<"ok">>};
        {error, RrdResponse} -> {RrdSensor, list_to_binary(RrdResponse)}
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

check_token(undefined, undefined) ->
    {false, false};
check_token(Token, undefined) ->
    check_sensor(Token);
check_token(undefined, Token) ->
    check_sensor(Token);
check_token(_, _) ->
    {false, false}.
