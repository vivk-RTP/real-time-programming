%%%-------------------------------------------------------------------
%%% @author Volcov Oleg
%%% @copyright (C) 2022, FAF-191
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(tcp_server_scaler).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_continue/2]).
-export([get_specs/0]).

-define(FREE_SOCKETS, 5).

-record(tcp_server_scaler_state, {}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	NewState = #tcp_server_scaler_state{},
	io:format("[~p] tcp_server_scaler is started.~n", [self()]),
	{ok, NewState, {continue, after_init}}.

handle_continue(after_init, State = #tcp_server_scaler_state{}) ->
	start_workers(?FREE_SOCKETS),
	{noreply, State}.

handle_call(_Request, _From, State = #tcp_server_scaler_state{}) ->
	{reply, ok, State}.

handle_cast({accept}, State = #tcp_server_scaler_state{}) ->
	start_workers(1),
	{noreply, State};
handle_cast(_Request, State = #tcp_server_scaler_state{}) ->
	{noreply, State}.

handle_info(_Info, State = #tcp_server_scaler_state{}) ->
	{noreply, State}.


%%%===================================================================
%%% External functions
%%%===================================================================

get_specs() ->
	#{
		id => tcp_server_scaler,
		start => {tcp_server_scaler, start_link, []},
		restart => permanent,
		shutdown => infinity,
		type => worker,
		modules => [tcp_server_scaler]
	}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

start_workers(Count) when Count =< 0 ->
	ok;
start_workers(Count) when Count > 0 ->
	tcp_socket_pool_sup:start_worker(),
	start_workers(Count-1).
