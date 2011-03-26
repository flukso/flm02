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
%% @doc Flukso API: /sensor resource specification 

-module(flukso_sensor).
-author('Bart Van Der Meerssche <bart.vandermeerssche@flukso.net>').

-export([init/1,
         allowed_methods/2,
         malformed_request/2,
         is_authorized/2,
         process_post/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include("flukso.hrl").

init([]) -> 
    {ok, undefined}.

% debugging
%init(Config) ->
%   {{trace, "/tmp"}, Config}.

allowed_methods(ReqData, State) ->
    {['POST'], ReqData, State}.

malformed_request(ReqData, State) ->
    {_Version, ValidVersion} = check_version(wrq:get_req_header("X-Version", ReqData)),
    {_Device, ValidDevice} = check_device(wrq:get_req_header("X-Device", ReqData)),
    {_Digest, ValidDigest} = check_digest(wrq:get_req_header("X-Digest", ReqData)),

    {case {ValidVersion, ValidDevice, ValidDigest} of
        {true, true, true} -> false;
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
    RrdResponse = [update_rrd(RrdSensor, proplists:get_value(RrdSensor, Measurements)) || RrdSensor <- Ids],

    JsonResponse = mochijson2:encode({struct, [{<<"response">>, {struct, RrdResponse}}]}),
    {true , wrq:set_resp_body(JsonResponse, ReqData), State}.

update_rrd(RrdSensor, TimeSeries) ->
    Path = "var/data/base/",
    RrdData = [[integer_to_list(Time), ":", integer_to_list(Counter), " "] || [Time, Counter] <- TimeSeries],

%debugging: io:format("~s~n", [[Path, [binary_to_list(RrdSensor)|".rrd"], " ", RrdData]]),
 
    case erlrrd:update([Path, [binary_to_list(RrdSensor)|".rrd"], " ", RrdData]) of
        {ok, _RrdResponse} -> {RrdSensor, <<"ok">>};
        {error, RrdResponse} -> {RrdSensor, list_to_binary(RrdResponse)}
    end.
