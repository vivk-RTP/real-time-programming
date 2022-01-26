%%%-------------------------------------------------------------------
%%% @author Volcov Oleg
%%% @copyright (C) 2022, FAF-191
%%% @doc Tweet-Analyzer application entry point.
%%% @end
%%%-------------------------------------------------------------------

-module(rtp_app).
-author("Volcov Oleg").

-behaviour(application).

%% OTP application entry point.
-export([start/2, stop/1]).
%% Tweet-Analyzer application entry point.
-export([start/0, stop/0]).

start() ->
    application:start(?MODULE).

start(_StartType, _StartArgs) ->
    io:format("[~p] Tweet-Analyzer's `start` is called.~n", [self()]),
    rtp_sup:start_link().

stop() ->
    application:stop(?MODULE).

stop(_State) ->
    io:format("[~p] Tweet-Analyzer's `stop` is called.~n", [self()]),
    ok.

