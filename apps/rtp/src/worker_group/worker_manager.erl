%%%-------------------------------------------------------------------
%%% @author Volcov Oleg
%%% @copyright (C) 2022, FAF-191
%%% @doc
%%% @end
%%%-------------------------------------------------------------------

-module(worker_manager).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([get_specs/0]).

-define(WORKER_SCALER, worker_scaler).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	{ok, 0}.

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast({tweet, _Tweet}, State) ->
	gen_server:cast(?WORKER_SCALER, {inc}),
	%% TODO: Find `worker` and send it
	{noreply, State};
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
