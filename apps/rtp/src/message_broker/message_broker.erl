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

handle_cast({subscribe, Attribute, DestinationPID}, State = #message_broker_state{subscribers = OldSubscribers}) ->
	Subscribers = subscribe(Attribute, DestinationPID, OldSubscribers),
	NewState = State#message_broker_state{subscribers = Subscribers},
	{noreply, NewState};
handle_cast({unsubscribe, Attribute, DestinationPID}, State = #message_broker_state{subscribers = OldSubscribers}) ->
	Subscribers = unsubscribe(Attribute, DestinationPID, OldSubscribers),
	NewState = State#message_broker_state{subscribers = Subscribers},
	{noreply, NewState};
handle_cast({publish, Attribute, Message}, State = #message_broker_state{subscribers = Subscribers}) ->
	publish(Attribute, Message, Subscribers),
	{noreply, State};
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

subscribe(Attribute, Subscriber, SubsMap) ->
	Found = maps:find(Attribute, SubsMap),
	add_subscriber(Found, Attribute, Subscriber, SubsMap).

add_subscriber({ok, OldSubs}, Attribute, Subscriber, SubsMap) ->
	SubsMap#{Attribute := [Subscriber|OldSubs]};
add_subscriber(error, Attribute, Subscriber, SubsMap) ->
	SubsMap#{Attribute => [Subscriber]}.

unsubscribe(Attribute, Subscriber, SubsMap) ->
	Found = maps:find(Attribute, SubsMap),
	remove_subscriber(Found, Attribute, Subscriber, SubsMap).

remove_subscriber({ok, OldSubs}, Attribute, Subscriber, SubsMap) ->
	Subs = lists:delete(Subscriber, OldSubs),
	SubsMap#{Attribute := Subs};
remove_subscriber(error, _, _, SubsMap) ->
	SubsMap.

publish(Attribute, Message, SubsMap) ->
	Found = maps:find(Attribute, SubsMap),
	send_message(Found, Attribute, Message).

send_message({ok, Subs}, Attribute, Message) ->
	send_message_loop(Subs, Attribute, Message);
send_message(error, _, _) ->
	ok.

send_message_loop([], _, _) ->
	ok;
send_message_loop([Head|Tail], Attribute, Message) ->
	gen_server:cast(Head, {Attribute, Message}),
	send_message_loop(Tail, Attribute, Message).


