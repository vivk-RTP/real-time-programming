%%%-------------------------------------------------------------------
%%% @author Volcov Oleg
%%% @copyright (C) 2022, FAF-191
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(attribute).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([get_specs/0]).

-record(attribute_state, {list}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
	gen_server:start_link(?MODULE, [], []).

init([]) ->
	NewState = #attribute_state{list = []},
	{ok, NewState}.

handle_call(_Request, _From, State = #attribute_state{}) ->
	{reply, ok, State}.

handle_cast({subscribe, PID}, State = #attribute_state{list = List}) ->
	Contains = lists:member(PID, List),
	NewState = subscribe(Contains, PID, State),
	io:format("[~p] attribute subscribe with NewState=[~p].~n", [self(), NewState]),
	{noreply, NewState};
handle_cast({unsubscribe, PID}, State = #attribute_state{list = List}) ->
	Contains = lists:member(PID, List),
	NewState = unsubscribe(Contains, PID, State),
	io:format("[~p] attribute unsubscribe with NewState=[~p].~n", [self(), NewState]),
	{noreply, NewState};
handle_cast({publish, Message}, State = #attribute_state{list = List}) ->
	io:format("[~p] attribute publish with List=[~p].~n", [self(), List]),
	publish_loop(Message, List),
	{noreply, State};
handle_cast(_Request, State = #attribute_state{}) ->
	{noreply, State}.

handle_info(_Info, State = #attribute_state{}) ->
	{noreply, State}.

%%%===================================================================
%%% External functions
%%%===================================================================

get_specs() ->
	#{
		id => attribute,
		start => {attribute, start_link, []},
		restart => temporary,
		shutdown => infinity,
		type => worker,
		modules => [attribute]
	}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

subscribe(true, _PID, State = #attribute_state{list = _PIDs}) ->
	State;
subscribe(false, PID, State = #attribute_state{list = PIDs}) ->
	State#attribute_state{list = [PID|PIDs]}.

unsubscribe(false, _PID, State = #attribute_state{list = _PIDs}) ->
	State;
unsubscribe(true, PID, State = #attribute_state{list = PIDs}) ->
	UnsubscribedList = lists:delete(PID, PIDs),
	State#attribute_state{list = UnsubscribedList}.

publish_loop(Message, [] = _PIDs) ->
	{ok, Message};
publish_loop(Message, [Head|Tail] = _PIDs) ->
	tcp_socket:send_data(Head, Message),
	publish_loop(Message, Tail).