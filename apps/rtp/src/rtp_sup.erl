%%%-------------------------------------------------------------------
%%% @author Volcov Oleg
%%% @copyright (C) 2022, FAF-191
%%% @doc Tweet-Analyzer main supervisor.
%%% @end
%%%-------------------------------------------------------------------

-module(rtp_sup).
-author("Volcov Oleg").

-behaviour(supervisor).

-define(TWEET_1, "http://127.0.0.1:4000/tweets/1").

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    io:format("[~p] Tweet-Analyzer' superviser's `init` is called.~n", [self()]),

    MaxRestarts = 100,
    MaxSecondsBetweenRestarts = 10,
    SupFlags = #{
        strategy => one_for_one,
        intensity => MaxRestarts,
        period => MaxSecondsBetweenRestarts
    },

    RequestSender = request_sender:get_specs(0, ?TWEET_1),
    ChildSpecs = [
        RequestSender
    ],
    {ok, {SupFlags, ChildSpecs}}.
