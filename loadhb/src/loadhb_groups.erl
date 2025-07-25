%%%-------------------------------------------------------------------
%%% @doc HyperBEAM Environment Configuration Module
%%% 
%%% This module handles reading and parsing environment configurations
%%% from the hb_envs.config file.
%%% 
%%% @end
%%%-------------------------------------------------------------------
-module(loadhb_groups).
-export([get_group/1, run_flows/1]).

-type env_name() :: atom().
-type url_type() :: router | compute | greenzone.
-type url_config() :: #{url => binary(), type => url_type()}.
-type flow_module() :: atom().
-type group() :: #{
    name => binary(),
    urls => [url_config()],
    flows => [flow_module()]
}.

-spec get_group(env_name()) -> {ok, group()} | {error, not_found}.
get_group(GroupName) ->
    case file:consult("loadhb_groups.config") of
        {ok, [{groups, Groups}]} ->
            case lists:keyfind(GroupName, 1, Groups) of
                {_GroupName, Group} ->
                    {ok, Group};
                false ->
                    {error, not_found}
            end;
        {error, Reason} ->
            {error, {config_error, Reason}}
    end.

-spec run_flows(group()) -> ok | {error, term()}.
run_flows(#{flows := Flows}) ->
    case hb_http_client:start_link(#{}) of
        {ok, _Pid} ->
            run_flows_list(Flows);
        {error, {already_started, _Pid}} ->
            run_flows_list(Flows);
        {error, Reason} ->
            {error, {http_client_start_failed, Reason}}
    end.

-spec run_flows_list([flow_module()]) -> ok | {error, term()}.
run_flows_list([]) ->
    ok;
run_flows_list([Flow | Rest]) ->
    try
        case Flow:run() of
            ok ->
                run_flows_list(Rest);
            {error, Reason} ->
                {error, {flow_failed, Flow, Reason}}
        end
    catch
        Error:CatchReason:_Stacktrace ->
            {error, {flow_crashed, Flow, Error, CatchReason}}
    end.