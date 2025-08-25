%%%-------------------------------------------------------------------
%%% @doc HyperBEAM Integration Test Module
%%% 
%%% This module contains integration tests for HyperBEAM, evaluating
%%% simple genesis_wasm functionality.
%%% 
%%% @end
%%%-------------------------------------------------------------------
-module(flow_genesis_eval).
-export([run/0, run/1]).

run() ->
    run(prod_basic).

run(GroupName) ->
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
        
        {ok, Url} = loadhb_groups:get_url(GroupName, compute),
        
        Result = hb_client:send_message(
            Url, 
            Message, 
            Wallet, 
            #{}
        ),
        
        HandledResult = handle_test_result(Result),

        loadhb_report:report({flow_genesis_eval, HandledResult})
    catch
        Error:Reason:_Stacktrace ->
            ErrorMsg = {Error, Reason},
            loadhb_report:report({flow_genesis_eval, [{error, ErrorMsg}]})
    end,

    ok.


handle_test_result({ok, #{status := 200, body := _Body}}) ->
    {ok, [
      <<"Successfully built a genesis wasm request">>,
      <<"Successfully evaluated a genesis wasm request">>
    ]};
handle_test_result({ok, #{status := Status, body := _Body}}) ->
    {error, [<<"Error: status=", (integer_to_binary(Status))/binary>>]};
handle_test_result({error, Reason}) ->
    {error, [Reason]};
handle_test_result(Other) ->
    {error, [{unexpected_result, Other}]}.