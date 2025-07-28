%%%-------------------------------------------------------------------
%%% @doc HyperBEAM Integration Test Module
%%% 
%%% This module contains integration tests for HyperBEAM, evaluating
%%% cron functionality
%%% 
%%% @end
%%%-------------------------------------------------------------------
-module(flow_genesis_cron).
-export([run/0]).

run() ->
    try
        Wallet = ar_wallet:new(),
        
        ProcessId = <<"Kv6jQCcs8GwNpioj6tkTt06zD130YgqIHX7QNnZQYQc">>,
        CronPath = <<"/", ProcessId/binary, "~process@1.0/now">>,
        Path = <<"/~cron@1.0/once?cron-path=", CronPath/binary>>,
        
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
        
        HandledCronResult = handle_cron_result(Result),

        case HandledCronResult of 
          {ok, Messages} ->
            start_monitoring(ProcessId, Wallet, Messages);
          {error, _Messages} ->
            loadhb_report:report({flow_genesis_eval, HandledCronResult})
        end
    catch
        Error:Reason:_Stacktrace ->
            ErrorMsg = {Error, Reason},
            loadhb_report:report({flow_genesis_eval, [{error, ErrorMsg}]})
    end,

    ok.


handle_cron_result({ok, #{status := 200, body := _Body}}) ->
    {ok, [
      <<"Successfully built a cron request">>,
      <<"Successfully evaluated a cron request">>
    ]};
handle_cron_result({ok, #{status := Status, body := Body}}) ->
    {error, [
      <<"Error: status=", (integer_to_binary(Status))/binary>>,
      <<"Error: body=", Body/binary>>
    ]};
handle_cron_result({error, Reason}) ->
    {error, [Reason]};
handle_cron_result(Other) ->
    {error, [{unexpected_result, Other}]}.


start_monitoring(ProcessId, Wallet, CronReqMessages) ->
    Path = <<"/", ProcessId/binary, "~process@1.0/compute/at-slot">>,
          
    Message = #{
        <<"path">> => Path,
        <<"method">> => <<"POST">>,
        <<"target">> => ProcessId,
        <<"accept-bundle">> => <<"true">>,
        <<"accept-codec">> => <<"httpsig@1.0">>,
        <<"signingFormat">> => <<"ANS-104">>
    },
    
    monitoring_loop(Message, Wallet, CronReqMessages, 1, undefined, []).

monitoring_loop(_Message, _Wallet, CronReqMessages, 4, _PrevBody, IterationMessages) ->
    AllMessages = CronReqMessages ++ IterationMessages,
    loadhb_report:report({flow_genesis_eval, {ok, AllMessages}});

monitoring_loop(Message, Wallet, CronReqMessages, Iteration, PrevBody, IterationMessages) ->
    Result = hb_client:send_message(
        <<"http://localhost:8734">>, 
        Message, 
        Wallet, 
        #{}
    ),
    
    case Result of 
        {ok, #{status := 200, body := Body}} ->
            case check_body_progress(Body, PrevBody) of
                {continue, NewIterationMessages} ->
                    timer:sleep(1000),
                    monitoring_loop(
                      Message, 
                      Wallet, 
                      CronReqMessages, 
                      Iteration + 1, 
                      Body, 
                      IterationMessages ++ NewIterationMessages
                    );
                {stop_success, NewIterationMessages} ->
                    AllMessages = CronReqMessages ++ IterationMessages ++ NewIterationMessages,
                    loadhb_report:report({flow_genesis_eval, {ok, AllMessages}})
            end;
        {ok, #{status := Status, body := Body}} ->
            IterBin = integer_to_binary(Iteration),
            StatusBin = integer_to_binary(Status),
            ErrorMsg = <<"Error: iteration ", IterBin/binary, 
                         " status=", StatusBin/binary, 
                         " body=", Body/binary>>,
            AllMessages = CronReqMessages ++ IterationMessages ++ [ErrorMsg],
            loadhb_report:report({flow_genesis_eval, {error, AllMessages}});
        {error, Reason} ->
            IterBin = integer_to_binary(Iteration),
            ReasonBin = list_to_binary(io_lib:format("~p", [Reason])),
            ErrorMsg = <<"Error: iteration ", IterBin/binary, 
                         " reason=", ReasonBin/binary>>,
            AllMessages = CronReqMessages ++ IterationMessages ++ [ErrorMsg],
            loadhb_report:report({flow_genesis_eval, {error, AllMessages}});
        Other ->
            IterBin = integer_to_binary(Iteration),
            OtherBin = list_to_binary(io_lib:format("~p", [Other])),
            ErrorMsg = <<"Error: iteration ", IterBin/binary, 
                         " unexpected result=", OtherBin/binary>>,
            AllMessages = CronReqMessages ++ IterationMessages ++ [ErrorMsg],
            loadhb_report:report({flow_genesis_eval, {error, AllMessages}})
    end.

check_body_progress(Body, undefined) ->
    case Body of
        <<"307">> ->
            {stop_success, [<<"Iteration 1: reached highest slot (307)">>]};
        _ ->
            {continue, [<<"Iteration 1: body=", Body/binary>>]}
    end;
check_body_progress(Body, PrevBody) ->
    case Body of
        <<"307">> ->
            {stop_success, [<<"Body reached highest slot (307)">>]};
        _ ->
            try
                BodyNum = binary_to_integer(Body),
                PrevBodyNum = binary_to_integer(PrevBody),
                if 
                    BodyNum > PrevBodyNum ->
                        Msg = <<"Body increased from ", PrevBody/binary, 
                                " to ", Body/binary>>,
                        {continue, [Msg]};
                    true ->
                        {continue, [<<"Body unchanged: ", Body/binary>>]}
                end
            catch
                _:_ ->
                    {continue, [<<"Body (non-numeric): ", Body/binary>>]}
            end
    end.