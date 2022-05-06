%%%-------------------------------------------------------------------
%%% @author Volcov Oleg
%%% @copyright (C) 2022, FAF-191
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(tcp_subscriber).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, handle_continue/2]).
-export([get_specs/0, subscribe/2, unsubscribe/1, publish/2]).

-record(tcp_subscriber_state, {socket, subscriber}).

-define(ADDRESS, "127.0.0.1").
-define(PORT, 25250).
-define(OPTIONS, [{active, true}, {packet, 0}]).

-define(SUBSCRIBE, "subscribe").
-define(UNSUBSCRIBE, "unsubscribe").
-define(PUBLISH, "publish").

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	io:format("[~p] tcp_subscriber's `init` is called.~n", [self()]),

	{ok, Socket} = gen_tcp:connect(?ADDRESS, ?PORT, ?OPTIONS),
	io:format("[~p] tcp_subscriber connect to Socket=[~p].~n", [self(), Socket]),

	NewState = #tcp_subscriber_state{socket = Socket},
	{ok, NewState, {continue, after_init}}.

handle_continue(after_init, State = #tcp_subscriber_state{socket = _Socket}) ->
	{noreply, State}.

handle_call(_Request, _From, State = #tcp_subscriber_state{}) ->
	{reply, ok, State}.

handle_cast({subscribe, PID}, State = #tcp_subscriber_state{}) ->
	NewState = State#tcp_subscriber_state{subscriber = PID},
	{noreply, NewState};
handle_cast({send, Message}, State = #tcp_subscriber_state{socket = Socket}) ->
	gen_tcp:send(Socket, Message),
	{noreply, State};
handle_cast(_Request, State = #tcp_subscriber_state{}) ->
	{noreply, State}.

handle_info({tcp, _, RawData}, State = #tcp_subscriber_state{subscriber = Subscriber}) ->
	client:process_data(RawData, Subscriber),
	{noreply, State};
handle_info({tcp_closed, Socket}, State = #tcp_subscriber_state{}) ->
	io:format("[~p] tcp_subscriber is closed from Socket=[~p].~n", [self(), Socket]),
	{stop, normal, State};
handle_info(_Info, State = #tcp_subscriber_state{}) ->
	{noreply, State}.

terminate(_Reason, _State = #tcp_subscriber_state{socket = Socket}) ->
	gen_tcp:close(Socket),
	ok.

%%%===================================================================
%%% External functions
%%%===================================================================

get_specs() ->
	#{
		id => tcp_subscriber,
		start => {tcp_subscriber, start_link, []},
		restart => transient,
		type => worker,
		modules => [tcp_subscriber]
	}.

subscribe(Attribute, PID) ->
	EncodedJSON = to_json(Attribute, ?SUBSCRIBE, undefined),
	gen_server:cast(tcp_subscriber, {subscribe, PID}),
	gen_server:cast(tcp_subscriber, {send, EncodedJSON}).

unsubscribe(Attribute) ->
	EncodedJSON = to_json(Attribute, ?UNSUBSCRIBE, undefined),
	gen_server:cast(tcp_subscriber, {send, EncodedJSON}).

publish(Attribute, Message) ->
	EncodedJSON = to_json(Attribute, ?PUBLISH, Message),
	gen_server:cast(tcp_subscriber, {send, EncodedJSON}).

%%%===================================================================
%%% Internal functions
%%%===================================================================

to_json(Attribute, CMD, Param) ->
	jsx:encode(
		#{
			<<"topic">> => Attribute,
			<<"command">> => CMD,
			<<"param">> => Param
		}).