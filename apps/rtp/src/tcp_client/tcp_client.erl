%%%-------------------------------------------------------------------
%%% @author Volcov Oleg
%%% @copyright (C) 2022, FAF-191
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(tcp_client).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, handle_continue/2]).
-export([get_specs/0, subscribe/1, unsubscribe/1, publish/2]).

-record(tcp_client_state, {socket}).

-define(ADDRESS, "127.0.0.1").
-define(PORT, 25255).
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
	io:format("~n~n[~p] tcp_client called init.~n~n~n", [self()]),
	NewState = #tcp_client_state{},
	{ok, NewState, {continue, after_init}}.

handle_continue(after_init, State = #tcp_client_state{socket = _Socket}) ->
	{noreply, State}.

handle_call(_Request, _From, State = #tcp_client_state{}) ->
	{reply, ok, State}.

handle_cast({send, Message}, State = #tcp_client_state{}) ->
	{ok, Socket} = gen_tcp:connect(?ADDRESS, ?PORT, ?OPTIONS),
	io:format("~n~n[~p] tcp_socket send Message from Socket=[~p].~n", [self(), Socket]),

	gen_tcp:send(Socket, Message),
	io:format("[~p] tcp_socket ACK from Socket=[~p].~n~n~n", [self(), Socket]),

	gen_tcp:close(Socket),
	{noreply, State};
handle_cast(_Request, State = #tcp_client_state{}) ->
	{noreply, State}.

handle_info({tcp, Socket, RawData}, State = #tcp_client_state{}) ->
	io:format("[~p] tcp_socket get Message=[~p] from Socket=[~p].~n", [self(), RawData, Socket]),
	{noreply, State};
handle_info({tcp_closed, Socket}, State = #tcp_client_state{}) ->
	io:format("[~p] tcp_client is closed from Socket=[~p].~n", [self(), Socket]),
	{stop, normal, State};
handle_info(_Info, State = #tcp_client_state{}) ->
	{noreply, State}.

terminate(_Reason, _State = #tcp_client_state{socket = Socket}) ->
	gen_tcp:close(Socket),
	ok.

%%%===================================================================
%%% External functions
%%%===================================================================

get_specs() ->
	#{
		id => tcp_client,
		start => {tcp_client, start_link, []},
		restart => transient,
		type => worker,
		modules => [tcp_client]
	}.

subscribe(Attribute) ->
	EncodedJSON = to_json(Attribute, ?SUBSCRIBE, undefined),
	gen_server:cast(tcp_client, {send, EncodedJSON}).

unsubscribe(Attribute) ->
	EncodedJSON = to_json(Attribute, ?UNSUBSCRIBE, undefined),
	gen_server:cast(tcp_client, {send, EncodedJSON}).

publish(Attribute, Message) ->
	EncodedJSON = to_json(Attribute, ?PUBLISH, Message),
	gen_server:cast(tcp_client, {send, EncodedJSON}).

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