%%%-------------------------------------------------------------------
%%% @author Volcov Oleg
%%% @copyright (C) 2022, FAF-191
%%% @doc Actor to receive `JSON data` from `sse_handler`,
%%%         notify ‘worker_scaler’ about new message,
%%%         choose specific ‘worker’ by ‘round-robin distribution’
%%%         and send `JSON data` to this ‘worker’.
%%% @end
%%%-------------------------------------------------------------------

-module(worker_manager).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([get_specs/0]).

-define(WORKER_SCALER, worker_scaler).
-define(WORKER_SUP, worker_sup).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
	io:format("[~p] worker_manager's `init` with is called.~n", [self()]),
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	{ok, 0}.

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast({tweet, Tweet}, State) ->
	gen_server:cast(?WORKER_SCALER, {inc}),
	WorkerPIDs = supervisor:which_children(?WORKER_SUP),
%%	WorkerCount = length(WorkerPIDs),

%%	NewIndex = round_robin_distribution(State, WorkerCount),
%%
%%	NthResult = lists:nth(NewIndex, WorkerPIDs),
%%	{_, WorkerPID, _, _} = NthResult,
	WorkerPID  = least_connected_balancing(WorkerPIDs),

	gen_server:cast(WorkerPID, {add_tweet, Tweet}),
	{noreply, 0};
handle_cast(_Request, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

%%%===================================================================
%%% External functions
%%%===================================================================

get_specs() ->
	#{
		id => worker_manager,
		start => {worker_manager, start_link, []},
		restart => permanent,
		shutdown => infinity,
		type => worker,
		modules => [worker_manager]
	}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

round_robin_distribution(_Index, _Length) when _Index < _Length ->
	_Index + 1;
round_robin_distribution(_Index, _Length) ->
	1.

least_connected_balancing([], _Min, MinPID) ->
	MinPID;
least_connected_balancing([Head|Tail], Min, MinPID) ->
	{_, WorkerPID, _, _} = Head,
	{amount, Amount} = gen_server:call(WorkerPID, {get_amount}),
	{Min, MinPID} = calculate_min(Min, MinPID, Amount, WorkerPID),
	least_connected_balancing(Tail, Min, MinPID).

least_connected_balancing(WorkerPIDs) ->
	[Head|Tail] = WorkerPIDs,
	{_, WorkerPID, _, _} = Head,
	{amount, Amount} = gen_server:call(WorkerPID, {get_amount}),
	least_connected_balancing(Tail, Amount, WorkerPID).

calculate_min(Min, _MinPID, Current, _CurrentPID) when Min =:= -1 ->
	{Current, _CurrentPID};
calculate_min(Min, _MinPID, Current, _CurrentPID) when Min > Current ->
	{Current, _CurrentPID};
calculate_min(Min, _MinPID, Current, _CurrentPID) when Min =< Current ->
	{Min, _MinPID}.
