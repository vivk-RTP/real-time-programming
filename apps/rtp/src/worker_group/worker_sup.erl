%%%-------------------------------------------------------------------
%%% @author Volcov Oleg
%%% @copyright (C) 2022, FAF-191
%%% @doc Supervisor for starting new and stopping useless `workers`.
%%% @end
%%%-------------------------------------------------------------------

-module(worker_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).
-export([get_specs/0, start_worker/1, stop_worker/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    MaxRestarts = 100,
    MaxSecondsBetweenRestarts = 10,
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => MaxRestarts,
        period => MaxSecondsBetweenRestarts
    },

    Worker = worker:get_specs(),

    Children = [
        Worker
    ],

    {ok, {SupFlags, Children}}.

%%%===================================================================
%%% External functions
%%%===================================================================

get_specs() ->
    #{
        id => worker_sup,
        start => {worker_sup, start_link, []},
        restart => permanent,
        shutdown => infinity,
        type => supervisor,
        modules => [worker_sup]
    }.

start_worker(Count) when Count > 0 ->
    {ok, _Pid} = supervisor:start_child(?MODULE, []),
    start_worker(Count-1);
start_worker(Count) when Count =< 0 ->
    ok.

stop_worker(Count) when Count > 0 ->
    WorkerPIDs = supervisor:which_children(?MODULE),
    stop_worker(WorkerPIDs, Count);
stop_worker(Count) when Count =< 0 ->
    ok.

stop_worker(_WorkerPIDs, Count) when Count =:= 0 ->
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
