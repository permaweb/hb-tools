%%%-------------------------------------------------------------------
%%% @doc LoadHB Main Entry Point
%%% 
%%% Main entry point for running HyperBEAM tests in different groups.
%%% Takes an group name as argument and executes the configured flows.
%%% 
%%% @end
%%%-------------------------------------------------------------------
-module(loadhb_main).
-export([main/1]).

-spec main([string()]) -> ok | {error, term()}.
main([]) ->
    {error, no_group_specified};
main([GroupNameStr | _]) ->
    GroupName = list_to_atom(GroupNameStr),
    
    case loadhb_groups:get_group(GroupName) of
        {ok, Group} ->
            loadhb_groups:run_flows(Group);
        {error, not_found} ->
            {error, group_not_found};
        {error, Reason} ->
            {error, Reason}
    end.