%%%-------------------------------------------------------------------
%%% @author Volcov Oleg
%%% @copyright (C) 2022, FAF-191
%%% @doc
%%% @end
%%%-------------------------------------------------------------------

-module(worker_scaler).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([get_specs/0]).

-define(ONE_SECOND, 1000).

-record(worker_scaler_state, {current, prev_average}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	erlang:send_after(?ONE_SECOND, self(), trigger),
	{ok, #worker_scaler_state{}}.

handle_call(_Request, _From, State = #worker_scaler_state{}) ->
	{reply, ok, State}.

handle_cast(_Request, State = #worker_scaler_state{}) ->
	{noreply, State}.

handle_info(trigger, State = #worker_scaler_state{}) ->
	erlang:send_after(?ONE_SECOND, self(), trigger),
	{noreply, State};
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

