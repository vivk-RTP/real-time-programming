%%%-------------------------------------------------------------------
%%% @author Volcov Oleg
%%% @copyright (C) 2022, FAF-191
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(message_broker).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([get_specs/0]).

-record(message_broker_state, {subscribers}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
	io:format("[~p] message_broker's `init` with is called.~n", [self()]),
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	NewState = #message_broker_state{subscribers = #{}},
	{ok, NewState}.

handle_call(_Request, _From, State = #message_broker_state{}) ->
	{reply, ok, State}.

handle_cast(_Request, State = #message_broker_state{}) ->
	{noreply, State}.

handle_info(_Info, State = #message_broker_state{}) ->
	{noreply, State}.

%%%===================================================================
%%% External functions
%%%===================================================================

get_specs() ->
	#{
		id => message_broker,
		start => {message_broker, start_link, []},
		restart => permanent,
		shutdown => infinity,
		type => worker,
		modules => [message_broker]
	}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
