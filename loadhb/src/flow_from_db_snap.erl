%%%-------------------------------------------------------------------
%%% @doc HyperBEAM Integration Test Module
%%% 
%%% @end
%%%-------------------------------------------------------------------
-module(flow_from_db_snap).
-export([run/0]).

run() ->
    try
        Wallet = ar_wallet:new(),
        
        ProcessId = <<"8sCebZHN8C5eheRWF243TFCF84OLqGWBYP8PdKf8NK8">>,
        Path = <<"/", ProcessId/binary, "~process@1.0/compute&slot=80/at-slot">>,
        
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
        
        HandledResult = handle_test_result(Result),

        loadhb_report:report({flow_from_db_snap, HandledResult})
    catch
        Error:Reason:_Stacktrace ->
            ErrorMsg = {Error, Reason},
            loadhb_report:report({flow_from_db_snap, [{error, ErrorMsg}]})
    end,

    ok.


handle_test_result({ok, #{status := 200, body := _Body}}) ->
    {ok, [
      <<"Successfully built a genesis wasm request, on top of a database snapshot">>,
      <<"Successfully evaluated a genesis wasm request, on top of a database snapshot">>
    ]};
handle_test_result({ok, #{status := Status, body := _Body}}) ->
    {error, [<<"Error: status=", (integer_to_binary(Status))/binary>>]};
handle_test_result({error, Reason}) ->
    {error, [Reason]};
handle_test_result(Other) ->
    {error, [{unexpected_result, Other}]}.