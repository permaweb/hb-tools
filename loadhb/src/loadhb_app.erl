%%%-------------------------------------------------------------------
%% @doc loadhb public API
%% @end
%%%-------------------------------------------------------------------

-module(loadhb_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    loadhb_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
