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
    io:format("Usage: loadhb_main <environment>~n"),
    io:format("Available environments: local~n"),
    {error, no_environment_specified};
main([EnvNameStr | _]) ->
    EnvName = list_to_atom(EnvNameStr),
    io:format("Starting LoadHB with environment: ~p~n", [EnvName]),
    
    case hb_envs:get_environment(EnvName) of
        {ok, Environment} ->
            io:format("Environment configuration: ~p~n", [Environment]),
            hb_envs:run_flows(Environment);
        {error, not_found} ->
            io:format("Environment '~p' not found in configuration~n", [EnvName]),
            {error, environment_not_found};
        {error, Reason} ->
            io:format("Failed to load environment configuration: ~p~n", [Reason]),
            {error, Reason}
    end.