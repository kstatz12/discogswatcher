%%%-------------------------------------------------------------------
%% @doc discogswatcher public API
%% @end
%%%-------------------------------------------------------------------

-module(discogswatcher_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    application:start(yamerl),
    discogswatcher_sup:start_link().



stop(_State) ->
    ok.

%% internal functions
