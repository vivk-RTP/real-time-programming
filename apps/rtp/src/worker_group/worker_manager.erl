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

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, handle_continue/2]).
-export([get_specs/2]).

-record(worker_manager_state, {sup_pid, worker_scaler, worker_sup, index}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link(SupPID) ->
	io:format("[~p] worker_manager's `init` with is called.~n", [self()]),
	gen_server:start_link(?MODULE, SupPID, []).

init(SupPID) ->
	gen_server:cast(message_broker, {subscribe, tweet, self()}),
	NewState = #worker_manager_state{sup_pid = SupPID, index = 0},
	{ok, NewState, {continue, after_init}}.

handle_continue(after_init, State = #worker_manager_state{sup_pid = SupPID}) ->
	WorkerSup = worker_group_utils:get_worker_sup(SupPID),
	WorkerScaler = worker_group_utils:get_scaler(SupPID),

	NewState = State#worker_manager_state{
		worker_sup = WorkerSup,
		worker_scaler = WorkerScaler},
	{noreply, NewState}.

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast({tweet, Tweet}, State = #worker_manager_state{
	worker_scaler = WorkerScaler,
	worker_sup = WorkerSup,
	index = Index
}) ->
	gen_server:cast(WorkerScaler, {inc}),
	WorkerPIDs = supervisor:which_children(WorkerSup),
	WorkerCount = length(WorkerPIDs),

	NewIndex = round_robin_distribution(Index, WorkerCount),

	NthResult = lists:nth(NewIndex, WorkerPIDs),
	{_, WorkerPID, _, _} = NthResult,

	gen_server:cast(WorkerPID, {tweet, Tweet}),
	NewState = State#worker_manager_state{index = NewIndex},
	{noreply, NewState};
handle_cast(_Request, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	gen_server:cast(message_broker, {unsubscribe, tweet, self()}),
	ok.

%%%===================================================================
%%% External functions
%%%===================================================================

get_specs(SupPID, ID) ->
	#{
		id => list_to_atom(ID++"_worker_manager"),
		start => {worker_manager, start_link, [SupPID]},
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
