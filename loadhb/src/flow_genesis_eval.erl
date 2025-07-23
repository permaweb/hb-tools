%%%-------------------------------------------------------------------
%%% @doc HyperBEAM Integration Test Module
%%% 
%%% This module contains integration tests for HyperBEAM, evaluating
%%% simple genesis_wasm functionality.
%%% 
%%% @end
%%%-------------------------------------------------------------------
-module(flow_genesis_eval).
-export([run/0]).

-spec run() -> ok | {error, term()}.
run() ->
    
    % Using imported HyperBEAM modules - no need to initialize full application
    
    try
        Wallet = ar_wallet:new(),
        
        ProcessId = <<"Kv6jQCcs8GwNpioj6tkTt06zD130YgqIHX7QNnZQYQc">>,
        Path = <<"/", ProcessId/binary, "~process@1.0/compute&slot=4/at-slot">>,
        
        Message = #{
            <<"path">> => Path,
            <<"method">> => <<"POST">>,
            <<"target">> => ProcessId,
            <<"accept-bundle">> => <<"true">>,
            <<"accept-codec">> => <<"httpsig@1.0">>,
            <<"signingFormat">> => <<"ANS-104">>
        },
        
        Result = hb_client:send_message(
            <<"http://localhost:8734">>, 
            Message, 
            Wallet, 
            #{}
        ),
        
        handle_test_result(Result)
    catch
        Error:Reason:Stacktrace ->
            ErrorMsg = {Error, Reason},
            {error, ErrorMsg}
    end.

-spec handle_test_result({ok, map()} | {error, term()}) -> 
    ok | {error, term()}.
handle_test_result({ok, #{status := Status, body := Body}}) ->
    ok;
handle_test_result({error, Reason}) ->
    {error, Reason};
handle_test_result(Other) ->
    {error, {unexpected_result, Other}}.