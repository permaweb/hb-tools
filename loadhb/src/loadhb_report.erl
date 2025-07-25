%%%-------------------------------------------------------------------
%%% @doc LoadHB Report Module
%%% 
%%% This module provides formatted reporting for HyperBEAM test results.
%%% 
%%% @end
%%%-------------------------------------------------------------------
-module(loadhb_report).
-export([report/1]).

%% @doc Generate a formatted report for flow test results
report({FlowName, {ok, Messages}}) ->
    print_header(),
    print_flow_name(FlowName),
    print_outcome(ok),
    print_messages(Messages),
    print_footer(),
    ok;

report({FlowName, {error, Messages}}) ->
    print_header(),
    print_flow_name(FlowName),
    print_outcome(error),
    print_messages(Messages),
    print_footer(),
    error.

%% Internal functions
print_header() ->
    io:format("~n-----------------------------------------------------------------------~n"),
    io:format("                          HYPERBEAM TEST REPORT                      ~n"),
    io:format("-----------------------------------------------------------------------~n").

print_flow_name(FlowName) ->
    FlowStr = atom_to_list(FlowName),
    io:format("Flow: ~-59s ~n", [FlowStr]).

print_outcome(ok) ->
    io:format("Status: PASSED~-52s ~n", [""]);
print_outcome(error) ->
    io:format("Status: FAILED~-52s ~n", [""]).

print_messages([]) ->
    io:format("Messages: (none)~-49s ~n", [""]);
print_messages(Messages) ->
    io:format("Messages:~-57s ~n", [""]),
    lists:foreach(fun print_message/1, Messages).

print_message(Msg) when is_binary(Msg) ->
    MsgStr = binary_to_list(Msg),
    print_wrapped_message(MsgStr);
print_message(Msg) when is_list(Msg) ->
    print_wrapped_message(Msg);
print_message(Msg) ->
    MsgStr = io_lib:format("~p", [Msg]),
    print_wrapped_message(lists:flatten(MsgStr)).

print_wrapped_message(Msg) ->
    MaxWidth = 60,
    case length(Msg) =< MaxWidth of
        true ->
            io:format("   • ~-60s ~n", [Msg]);
        false ->
            {Line, Rest} = split_at_word_boundary(Msg, MaxWidth),
            io:format("   • ~-60s ~n", [Line]),
            print_continuation_lines(Rest, MaxWidth)
    end.

print_continuation_lines([], _) ->
    ok;
print_continuation_lines(Msg, MaxWidth) ->
    case length(Msg) =< MaxWidth of
        true ->
            io:format("     ~-62s ~n", [Msg]);
        false ->
            {Line, Rest} = split_at_word_boundary(Msg, MaxWidth),
            io:format("     ~-62s ~n", [Line]),
            print_continuation_lines(Rest, MaxWidth)
    end.

split_at_word_boundary(Msg, MaxWidth) ->
    case length(Msg) =< MaxWidth of
        true ->
            {Msg, []};
        false ->
            case string:rstr(string:substr(Msg, 1, MaxWidth), " ") of
                0 ->
                    % No space found, split at MaxWidth
                    {string:substr(Msg, 1, MaxWidth), string:substr(Msg, MaxWidth + 1)};
                Pos ->
                    % Split at last space
                    {string:substr(Msg, 1, Pos - 1), 
                     string:strip(string:substr(Msg, Pos + 1), left)}
            end
    end.

print_footer() ->
    io:format("----------------------------------------------------------------------~n~n").