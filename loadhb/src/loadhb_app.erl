%%%-------------------------------------------------------------------
%% @doc loadhb public API
%% @end
%%%-------------------------------------------------------------------

-module(loadhb_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    % Start the supervisor first
    {ok, Pid} = loadhb_sup:start_link(),
    % Initialize hb_event server to register prometheus counters
    % Use increment to trigger server initialization
    hb_event:increment(init, startup, #{}, 1),
    {ok, Pid}.

stop(_State) ->
    ok.

%% internal functions
