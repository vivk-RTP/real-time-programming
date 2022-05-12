%%%-------------------------------------------------------------------
%%% @author Volcov Oleg
%%% @copyright (C) 2022, FAF-191
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(message_broker_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(PORT, 25250).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    io:format("[~p] message_broker_sup superviser's `init` is called.~n", [self()]),

    MaxRestarts = 5000,
    MaxSecondsBetweenRestarts = 10,
    SupFlags = #{
        strategy => one_for_one,
        intensity => MaxRestarts,
        period => MaxSecondsBetweenRestarts
    },

    TCPServerSup = tcp_server_sup:get_specs(?PORT),
    AttributeSup = attribute_sup:get_specs(),

    ChildSpecs = [
        TCPServerSup,
        AttributeSup
    ],

    {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% External functions
%%%===================================================================



