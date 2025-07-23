%%%-------------------------------------------------------------------
%%% @doc LoadHB Main Entry Point
%%% 
%%% Main entry point for running HyperBEAM tests in different environments.
%%% Takes an environment name as argument and executes the configured flows.
%%% 
%%% @end
%%%-------------------------------------------------------------------
-module(loadhb_main).
-export([main/1]).

-spec main([string()]) -> ok | {error, term()}.
main([]) ->
    {error, no_environment_specified};
main([EnvNameStr | _]) ->
    EnvName = list_to_atom(EnvNameStr),
    
    case hb_envs:get_environment(EnvName) of
        {ok, Environment} ->
            hb_envs:run_flows(Environment);
        {error, not_found} ->
            {error, environment_not_found};
        {error, Reason} ->
            {error, Reason}
    end.