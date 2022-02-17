%%%-------------------------------------------------------------------
%%% @author Volcov Oleg
%%% @copyright (C) 2022, FAF-191
%%% @doc Calculate average count of needed workers and set it.
%%%
%%%      Count the number of messages to provide
%%%         by @inc async endpoint.
%%%      Every @INTERVAL of milliseconds count average
%%%         amount of messages and decide how many
%%%         workers is needed.
%%% @end
%%%-------------------------------------------------------------------

-module(worker_scaler).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([get_specs/0]).

-define(COUNT_OF_ITERATIONS, 10).
-define(WORKER_SUP, worker_sup).
-define(INTERVAL, 1000).

-define(START_WORKER_COUNT, 2000).

-record(worker_scaler_state, {current, prev_average}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	io:format("[~p] worker_scaler's `init` with is called.~n", [self()]),

	NewState = #worker_scaler_state{current = 0, prev_average = 0},

	set_workers(?START_WORKER_COUNT),

	erlang:send_after(?INTERVAL, self(), trigger),
	{ok, NewState}.

handle_call(_Request, _From, State = #worker_scaler_state{}) ->
	{reply, ok, State}.

handle_cast({inc}, State = #worker_scaler_state{}) ->
	{worker_scaler_state, Current, _} = State,
	NewState = State#worker_scaler_state{current = Current+1},
	{noreply, NewState};
handle_cast(_Request, State = #worker_scaler_state{}) ->
	{noreply, State}.

handle_info(trigger, State = #worker_scaler_state{}) ->
	{worker_scaler_state, Current, PrevAverage} = State,
	Diff = calculate_difference(Current),

	set_workers(Diff),

	NewState = State#worker_scaler_state{current = 0, prev_average = 0},
	io:format("[~p] worker_scaler's `re-scale` with Diff=~p and NewState=~p is called.~n", [self(), Diff, NewState]),

	erlang:send_after(?INTERVAL, self(), trigger),
	{noreply, NewState};
handle_info(_Info, State = #worker_scaler_state{}) ->
	{noreply, State}.

%%%===================================================================
%%% External functions
%%%===================================================================

get_specs() ->
	#{
		id => worker_scaler,
		start => {worker_scaler, start_link, []},
		restart => permanent,
		shutdown => infinity,
		type => worker,
		modules => [worker_scaler]
	}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

calculate_average(PrevAverage, NewAverage) when PrevAverage =:= 0 ->
	NewAverage;
calculate_average(PrevAverage, NewAverage) ->
	PrevAverage - (PrevAverage / ?COUNT_OF_ITERATIONS) + (NewAverage / ?COUNT_OF_ITERATIONS).

calculate_difference(Current) ->
	WorkerPIDs = supervisor:which_children(?WORKER_SUP),
	WorkersCount = length(WorkerPIDs),

	Current div 2 - WorkersCount.

set_workers(Diff) when Diff >= 0 ->
	worker_sup:start_worker(Diff);
set_workers(Diff) when Diff < 0 ->
	worker_sup:stop_worker(-Diff).