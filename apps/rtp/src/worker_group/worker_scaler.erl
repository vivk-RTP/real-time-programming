%%%-------------------------------------------------------------------
%%% @author Volcov Oleg
%%% @copyright (C) 2022, FAF-191
%%% @doc Actor to start new ‘Workers’ by ‘Worker Supervisor’
%%%         or stop useless. Actor will count messages in a time
%%%         interval and decide how many ‘Workers’ are enough.
%%% @end
%%%-------------------------------------------------------------------

-module(worker_scaler).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_continue/2]).
-export([get_specs/2]).

-define(COUNT_OF_ITERATIONS, 10).
-define(INTERVAL, 1000).

-define(START_WORKER_COUNT, 2000).

-record(worker_scaler_state, {current, sup_pid, worker_sup}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link(SupPID) ->
	gen_server:start_link(?MODULE, SupPID, []).

init(SupPID) ->
	io:format("[~p] worker_scaler's `init` with is called.~n", [self()]),

	NewState = #worker_scaler_state{
		current = 0,
		sup_pid = SupPID},

	erlang:send_after(?INTERVAL, self(), trigger),
	{ok, NewState, {continue, after_init}}.

handle_continue(after_init, State = #worker_scaler_state{sup_pid = SupPID}) ->
	WorkerSup = worker_group_utils:get_worker_sup(SupPID),
	set_workers(WorkerSup, ?START_WORKER_COUNT),

	NewState = State#worker_scaler_state{worker_sup = WorkerSup},
	{noreply, NewState}.

handle_call(_Request, _From, State = #worker_scaler_state{}) ->
	{reply, ok, State}.

handle_cast({inc}, State = #worker_scaler_state{current = Current}) ->
	NewState = State#worker_scaler_state{current = Current+1},
	{noreply, NewState};
handle_cast(_Request, State = #worker_scaler_state{}) ->
	{noreply, State}.

handle_info(trigger, State = #worker_scaler_state{current = Current, worker_sup = WorkerSup}) ->
	Diff = calculate_difference(Current, WorkerSup),

	set_workers(WorkerSup, Diff),

	NewState = State#worker_scaler_state{current = 0},
	io:format("~n[~p] worker_scaler's `re-scale` with `Current`=~p and `Diff`=~p is called.~n~n",
		[self(), Current, Diff]),

	erlang:send_after(?INTERVAL, self(), trigger),
	{noreply, NewState};
handle_info(_Info, State = #worker_scaler_state{}) ->
	{noreply, State}.

%%%===================================================================
%%% External functions
%%%===================================================================

get_specs(SupPID, ID) ->
	#{
		id => list_to_atom(ID++"_worker_scaler"),
		start => {worker_scaler, start_link, [SupPID]},
		restart => permanent,
		shutdown => infinity,
		type => worker,
		modules => [worker_scaler]
	}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

calculate_difference(Current, WorkerSup) ->
	WorkerPIDs = supervisor:which_children(WorkerSup),
	WorkersCount = length(WorkerPIDs),

	Current div 2 - WorkersCount.

set_workers(PID, Diff) when Diff >= 0 ->
	worker_sup:start_worker({PID, Diff});
set_workers(PID, Diff) when Diff < 0 ->
	worker_sup:stop_worker({PID, -Diff}).