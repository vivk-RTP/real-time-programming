%%%-------------------------------------------------------------------
%% @doc rtp public API
%% @end
%%%-------------------------------------------------------------------

-module(rtp_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    rtp_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
