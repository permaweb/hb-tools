%%%-------------------------------------------------------------------
%%% @doc HyperBEAM Environment Configuration Module
%%% 
%%% This module handles reading and parsing environment configurations
%%% from the hb_envs.config file.
%%% 
%%% @end
%%%-------------------------------------------------------------------
-module(loadhb_groups).
-export([get_group/1, run_flows/1, get_url/2]).

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
run_flows(#{flows := Flows, name := GroupName}) when is_binary(GroupName) ->
    run_flows_internal(Flows, binary_to_atom(GroupName));
run_flows(#{flows := Flows}) ->
    % Handle case where name might be missing - use default
    run_flows_internal(Flows, prod_basic).

run_flows_internal(Flows, GroupName) ->
    case hb_http_client:start_link(#{}) of
        {ok, _Pid} ->
            run_flows_list(Flows, GroupName);
        {error, {already_started, _Pid}} ->
            run_flows_list(Flows, GroupName);
        {error, Reason} ->
            {error, {http_client_start_failed, Reason}}
    end.

-spec run_flows_list([flow_module()], env_name()) -> ok | {error, term()}.
run_flows_list([], _GroupName) ->
    ok;
run_flows_list([Flow | Rest], GroupName) ->
    try
        case Flow:run(GroupName) of
            ok ->
                run_flows_list(Rest, GroupName);
            {error, Reason} ->
                {error, {flow_failed, Flow, Reason}}
        end
    catch
        Error:CatchReason:_Stacktrace ->
            {error, {flow_crashed, Flow, Error, CatchReason}}
    end.

-spec get_url(env_name(), url_type()) -> {ok, binary()} | {error, term()}.
get_url(GroupName, UrlType) ->
    case get_group(GroupName) of
        {ok, #{urls := Urls}} ->
            case lists:keyfind(UrlType, 2, [maps:to_list(Url) || Url <- Urls]) of
                false ->
                    case find_url_by_type(Urls, UrlType) of
                        {ok, Url} -> {ok, Url};
                        error -> {error, {url_not_found, UrlType}}
                    end;
                _ ->
                    case find_url_by_type(Urls, UrlType) of
                        {ok, Url} -> {ok, Url};
                        error -> {error, {url_not_found, UrlType}}
                    end
            end;
        {error, Reason} ->
            {error, Reason}
    end.

find_url_by_type([], _UrlType) ->
    error;
find_url_by_type([#{url := Url, type := UrlType} | _Rest], UrlType) when is_list(Url) ->
    {ok, list_to_binary(Url)};
find_url_by_type([#{url := Url, type := UrlType} | _Rest], UrlType) when is_binary(Url) ->
    {ok, Url};
find_url_by_type([_Other | Rest], UrlType) ->
    find_url_by_type(Rest, UrlType).