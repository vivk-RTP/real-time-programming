%%%-------------------------------------------------------------------
%%% @author Volcov Oleg
%%% @copyright (C) 2022, FAF-191
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(tcp_socket).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	handle_continue/2]).
-export([get_specs/1, send_data/2]).

-record(tcp_accept_socket_state, {socket, stash}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link(Socket) ->
	gen_server:start_link(?MODULE, Socket, []).

init(Socket) ->
	NewState = #tcp_accept_socket_state{socket = Socket, stash = []},
	io:format("[~p] tcp_socket is started with `Listener` Socket=[~p].~n", [self(), Socket]),
	{ok, NewState, {continue, after_init}}.

handle_continue(after_init, State = #tcp_accept_socket_state{socket = Socket}) ->
	{ok, NewSocket} = gen_tcp:accept(Socket),
	NewState = State#tcp_accept_socket_state{socket = NewSocket},
	gen_server:cast(tcp_server_scaler, {accept}),
	io:format("[~p] tcp_socket is accepted with `Accept` Socket=[~p].~n", [self(), NewSocket]),

	{noreply, NewState}.

handle_call(_Request, _From, State = #tcp_accept_socket_state{}) ->
	{reply, ok, State}.

handle_cast({send, Message}, State = #tcp_accept_socket_state{socket = Socket}) ->
	io:format("[~p] tcp_socket send Message to Socket=[~p].~n", [self(), Socket]),
	gen_tcp:send(Socket, Message),
	{noreply, State};
handle_cast(_Request, State = #tcp_accept_socket_state{}) ->
	{noreply, State}.

handle_info({tcp, _Socket, RawData}, State = #tcp_accept_socket_state{stash = Stash}) ->
	io:format("[~p] tcp_socket get Message from Socket=[~p].~n", [self(), _Socket]),
	NewStash = tcp_message_process_utils:process_tcp_data(RawData, Stash, self()),
	NewState = State#tcp_accept_socket_state{stash = NewStash},
	{noreply, NewState};
handle_info({tcp_closed, Socket}, State = #tcp_accept_socket_state{}) ->
	io:format("[~p] tcp_socket is closed from Socket=[~p].~n", [self(), Socket]),
	{stop, normal, State};
handle_info(_Info, State = #tcp_accept_socket_state{}) ->
	io:format("[~p] tcp_socket get unknown Message=[~p].~n", [self(), _Info]),
	{noreply, State}.

%%%===================================================================
%%% External functions
%%%===================================================================

get_specs(Socket) ->
	#{
		id => tcp_socket,
		start => {tcp_socket, start_link, [Socket]},
		restart => temporary,
		shutdown => infinity,
		type => worker,
		modules => [tcp_socket]
	}.

send_data(PID, Data) ->
	gen_server:cast(PID, {send, Data}).

%%%===================================================================
%%% Internal functions
%%%===================================================================
