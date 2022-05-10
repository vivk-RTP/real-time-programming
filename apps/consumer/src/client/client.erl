%%%-------------------------------------------------------------------
%%% @author Volcov Oleg
%%% @copyright (C) 2022, FAF-191
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(client).

-behaviour(gen_server).

-export([start_link/1, get_specs/1, process_data/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2]).

-record(client_state, {attribute, stash}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link(Attribute) ->
	gen_server:start_link(?MODULE, Attribute, []).

init(Attribute) ->
	tcp_subscriber:subscribe(Attribute, self()),
	NewState = #client_state{attribute = Attribute, stash = []},
	{ok, NewState}.

handle_call(_Request, _From, State = #client_state{}) ->
	{reply, ok, State}.

handle_cast({process, Data}, State = #client_state{stash = Stash, attribute = Attribute}) ->
	io:format("[~p] client Attribute=[~s] process data=[~p].~n", [self(), Attribute, Data]),
	{noreply, State};
handle_cast(_Request, State = #client_state{}) ->
	{noreply, State}.

handle_info(_Info, State = #client_state{}) ->
	{noreply, State}.

terminate(_Reason, _State = #client_state{attribute = Attribute}) ->
	tcp_subscriber:unsubscribe(Attribute),
	ok.

%%%===================================================================
%%% External functions
%%%===================================================================

get_specs(Attribute) ->
	#{
		id => list_to_atom("client_"++Attribute),
		start => {client, start_link, [Attribute]},
		restart => transient,
		type => worker,
		modules => [client]
	}.

process_data(Data, PID) ->
	gen_server:cast(PID, {process, Data}).

%%%===================================================================
%%% Internal functions
%%%===================================================================

is_json(LMessage) when is_list(LMessage) ->
	BMessage = list_to_binary(LMessage),
	is_json(BMessage);
is_json(BMessage) ->
	IsJson = jsx:is_json(BMessage),
	{BMessage, IsJson}.

decode_and_stash(Message, Stash, false) ->
	{undefined, Stash++Message};
decode_and_stash(Message, _Stash, true) ->
	Map = jsx:decode(Message),
	{Map, []}.

test_map(Map, _Attribute) when is_map(Map) =:= false ->
	ok;
test_map(Map, Attribute) ->
	io:format("[~p] client get Attribute=[~s] data from TCP=[~n~s~n].~n", [self(), Attribute, Map]).
