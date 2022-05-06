%%%-------------------------------------------------------------------
%%% @author Volcov Oleg
%%% @copyright (C) 2022, FAF-191
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(client).

-behaviour(gen_server).

-export([start_link/0, get_specs/0, process_data/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2]).

-define(ATTRIBUTE, "Users").

-record(client_state, {}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	tcp_subscriber:subscribe(?ATTRIBUTE, self()),
	{ok, #client_state{}}.

handle_call(_Request, _From, State = #client_state{}) ->
	{reply, ok, State}.

handle_cast({process, Data}, State = #client_state{}) ->
	io:format("[~p] client process data=[~p].~n", [self(), Data]),
	{noreply, State};
handle_cast(_Request, State = #client_state{}) ->
	{noreply, State}.

handle_info(_Info, State = #client_state{}) ->
	{noreply, State}.

terminate(_Reason, _State = #client_state{}) ->
	tcp_subscriber:unsubscribe(?ATTRIBUTE),
	ok.

%%%===================================================================
%%% External functions
%%%===================================================================

get_specs() ->
	#{
		id => client,
		start => {client, start_link, []},
		restart => transient,
		type => worker,
		modules => [client]
	}.

process_data(Data, PID) ->
	gen_server:cast(PID, {process, Data}).

%%%===================================================================
%%% Internal functions
%%%===================================================================
