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
    io:format("Running HyperBEAM integration test...~n"),
    
    % Using imported HyperBEAM modules - no need to initialize full application
    
    try
        io:format("Step 1: Creating wallet...~n"),
        Wallet = ar_wallet:new(),
        io:format("Step 2: Wallet created successfully~n"),
        
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
        
        io:format("Step 3: Calling hb_client:send_message...~n"),
        Result = hb_client:send_message(
            <<"http://localhost:8734">>, 
            Message, 
            Wallet, 
            #{}
        ),
        io:format("Step 4: send_message completed~n"),
        io:format("Step 5: Result is: ~p~n", [Result]),
        
        io:format("Step 6: Calling handle_test_result...~n"),
        handle_test_result(Result)
    catch
        Error:Reason:Stacktrace ->
            ErrorMsg = {Error, Reason},
            io:format("Integration test error: ~p:~p~n", [Error, Reason]),
            io:format("Stacktrace: ~p~n", [Stacktrace]),
            {error, ErrorMsg}
    end.

-spec handle_test_result({ok, map()} | {error, term()}) -> 
    ok | {error, term()}.
handle_test_result({ok, #{status := Status, body := Body}}) ->
    io:format("Step 7a: Success case - Transfer status: ~p~n", [Status]),
    io:format("Step 7b: Success case - Response: ~p~n", [Body]),
    io:format("Integration test completed successfully!~n"),
    ok;
handle_test_result({error, Reason}) ->
    io:format("Step 7c: Error case - Integration test failed: ~p~n", [Reason]),
    {error, Reason};
handle_test_result(Other) ->
    io:format("Step 7d: Unexpected result format: ~p~n", [Other]),
    {error, {unexpected_result, Other}}.