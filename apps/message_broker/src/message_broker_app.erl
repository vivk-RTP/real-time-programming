%%%-------------------------------------------------------------------
%% @doc message_broker public API
%% @end
%%%-------------------------------------------------------------------

-module(message_broker_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    message_broker_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
