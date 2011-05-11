%% @author Bart Van Der Meerssche <bart.vandermeerssche@flukso.net>
%% @copyright (C) 2011 Bart Van Der Meerssche
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
%% @doc Flukso API: /device/xyz resource specification 

-module(flukso_device_xyz).
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
    case wrq:method(ReqData) of
        'POST' -> malformed_POST(ReqData, State)
    end.

malformed_POST(ReqData, _State) ->
    {_Version, ValidVersion} = check_version(wrq:get_req_header("X-Version", ReqData)),
    {Device, ValidDevice} = check_device(wrq:path_info(device, ReqData)),
    {Digest, ValidDigest} = check_digest(wrq:get_req_header("X-Digest", ReqData)),

    State = #state{device = Device,
                   digest = Digest},

    {case {ValidVersion, ValidDevice, ValidDigest} of
        {true, true, true} -> false;
        _ -> true
     end,
    ReqData, State}.

is_authorized(ReqData, State) ->
    case wrq:method(ReqData) of
        'POST' -> is_auth_POST(ReqData, State)
    end.

is_auth_POST(ReqData, #state{device = Device, digest = ClientDigest} = State) ->
    {data, Result} = mysql:execute(pool, device_key, [Device]),

    case mysql:get_result_rows(Result) of
        [[Key]] ->
            Data = wrq:req_body(ReqData),
            <<X:160/big-unsigned-integer>> = crypto:sha_mac(Key, Data),
            ServerDigest = lists:flatten(io_lib:format("~40.16.0b", [X])),

            {case ServerDigest of
                 ClientDigest -> true;
                 _WrongDigest -> "Incorrect digest"
             end,
             ReqData, State};

        _NoKey ->
            {"No proper provisioning for this device", ReqData, State}
    end.

% JSON: {"memtotal":13572,"version":210,"memcached":3280,"membuffers":1076,"memfree":812,"uptime":17394,"reset":1}
% Mochijson2: {struct,[{<<"memtotal">>,   13572},
%                      {<<"version">>,      210},
%                      {<<"memcached">>,   3280},
%                      {<<"membuffers">>,  1076},
%                      {<<"memfree">>,      812},
%                      {<<"uptime">>,     17394},
%                      {<<"reset">>,          1}]}
process_post(ReqData, #state{device = Device} = State) ->
    {data, Result} = mysql:execute(pool, device_props, [Device]),
    [[Key, Upgrade, Resets]] = mysql:get_result_rows(Result),

    {struct, JsonData} = mochijson2:decode(wrq:req_body(ReqData)),

    Version = proplists:get_value(<<"version">>, JsonData),
    Reset = proplists:get_value(<<"reset">>, JsonData),
    Uptime = proplists:get_value(<<"uptime">>, JsonData),
    Memtotal = proplists:get_value(<<"memtotal">>, JsonData),
    Memcached = proplists:get_value(<<"memcached">>, JsonData),
    Membuffers = proplists:get_value(<<"membuffers">>, JsonData),
    Memfree = proplists:get_value(<<"memfree">>, JsonData),

    NewResets = Resets + Reset,

    mysql:execute(pool, device_update,
        [unix_time(), Version, 0, NewResets, Uptime, Memtotal, Memfree, Memcached, Membuffers, Device]),


    JsonResponse = mochijson2:encode({struct, [{<<"upgrade">>,       Upgrade},
                                               {<<"timestamp">>, unix_time()}
                                              ]}),

    <<X:160/big-unsigned-integer>> = crypto:sha_mac(Key, JsonResponse),
    Digest = lists:flatten(io_lib:format("~40.16.0b", [X])),

    DigestedReqData = wrq:set_resp_header("X-Digest", Digest, ReqData), 
    EmbodiedReqData = wrq:set_resp_body(JsonResponse, DigestedReqData),

    {true , EmbodiedReqData, State}.
