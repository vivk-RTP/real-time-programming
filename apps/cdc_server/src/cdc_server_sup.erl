%%%-------------------------------------------------------------------
%% @doc cdc_server top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(cdc_server_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(PORT, 25255).

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

    SinkSup = sink_sup:get_specs(),
    TCPServerSup = tcp_server_sup:get_specs(?PORT),
    TCPClient = tcp_client:get_specs(),


    ChildSpecs = [
        SinkSup,
        TCPServerSup,
        TCPClient
    ],

    {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% External functions
%%%===================================================================



