%%%-------------------------------------------------------------------
%%% @author Volcov Oleg
%%% @copyright (C) 2022, FAF-191
%%% @doc Actor to dynamically balance the amount of ‘Workers’.
%%% @end
%%%-------------------------------------------------------------------

-module(worker_sup).

-behaviour(supervisor).

-export([start_link/1, init/1]).
-export([get_specs/2, start_worker/1, stop_worker/1]).

-define(MINIMAL_WORKERS, 10).

start_link(WorkerSpec) ->
    supervisor:start_link(?MODULE, WorkerSpec).

init(WorkerSpec) ->
    MaxRestarts = 5000,
    MaxSecondsBetweenRestarts = 10,
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => MaxRestarts,
        period => MaxSecondsBetweenRestarts
    },

    Children = [
        WorkerSpec
    ],

    {ok, {SupFlags, Children}}.

%%%===================================================================
%%% External functions
%%%===================================================================

get_specs(ID, WorkerSpec) ->
    #{
        id => list_to_atom(ID++"_worker_sup"),
        start => {worker_sup, start_link, [WorkerSpec]},
        restart => permanent,
        shutdown => infinity,
        type => supervisor,
        modules => [worker_sup]
    }.

start_worker(Count) when Count > ?MINIMAL_WORKERS ->
    {ok, _Pid} = supervisor:start_child(?MODULE, []),
    start_worker(Count-1);
start_worker(Count) when Count =< ?MINIMAL_WORKERS ->
    ok.

stop_worker(Count) when Count > ?MINIMAL_WORKERS ->
    WorkerPIDs = supervisor:which_children(?MODULE),
    stop_worker(WorkerPIDs, Count);
stop_worker(Count) when Count =< ?MINIMAL_WORKERS ->
    ok.

stop_worker(_WorkerPIDs, Count) when Count =:= ?MINIMAL_WORKERS ->
    ok;
stop_worker([] = _WorkerPIDs, _Count) ->
    ok;
stop_worker([Head|Tails] = _WorkerPIDs, Count) ->
    {_, PID, _, _} = Head,
    supervisor:terminate_child(?MODULE, PID),
    stop_worker(Tails, Count-1).


%%%===================================================================
%%% Internal functions
%%%===================================================================
