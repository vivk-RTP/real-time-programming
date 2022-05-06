%%%-------------------------------------------------------------------
%% @doc consumer top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(consumer_sup).


-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    io:format("[~p] consumer superviser's `init` is called.~n", [self()]),

    MaxRestarts = 5000,
    MaxSecondsBetweenRestarts = 10,
    SupFlags = #{
        strategy => one_for_one,
        intensity => MaxRestarts,
        period => MaxSecondsBetweenRestarts
    },

    ChildSpecs = [

    ],

    {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% External functions
%%%===================================================================