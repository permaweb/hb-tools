%%%-------------------------------------------------------------------
%%% @doc HyperBEAM Environment Configuration Module
%%% 
%%% This module handles reading and parsing environment configurations
%%% from the hb_envs.config file.
%%% 
%%% @end
%%%-------------------------------------------------------------------
-module(hb_envs).
-export([get_environment/1, run_flows/1]).

-type env_name() :: atom().
-type url_type() :: router | compute | greenzone.
-type url_config() :: #{url => binary(), type => url_type()}.
-type flow_module() :: atom().
-type environment() :: #{
    name => binary(),
    urls => [url_config()],
    flows => [flow_module()]
}.

-spec get_environment(env_name()) -> {ok, environment()} | {error, not_found}.
get_environment(EnvName) ->
    case file:consult("hb_envs.config") of
        {ok, [{environments, Environments}]} ->
            case lists:keyfind(EnvName, 1, Environments) of
                {_EnvName, Environment} ->
                    {ok, Environment};
                false ->
                    {error, not_found}
            end;
        {error, Reason} ->
            {error, {config_error, Reason}}
    end.

-spec run_flows(environment()) -> ok | {error, term()}.
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