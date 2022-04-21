%%%-------------------------------------------------------------------
%%% @author Oleg
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(tcp_socket).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	code_change/3]).

-define(SERVER, ?MODULE).

-record(tcp_accept_socket_state, {}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
	{ok, #tcp_accept_socket_state{}}.

handle_call(_Request, _From, State = #tcp_accept_socket_state{}) ->
	{reply, ok, State}.

handle_cast(_Request, State = #tcp_accept_socket_state{}) ->
	{noreply, State}.

handle_info(_Info, State = #tcp_accept_socket_state{}) ->
	{noreply, State}.

terminate(_Reason, _State = #tcp_accept_socket_state{}) ->
	ok.

code_change(_OldVsn, State = #tcp_accept_socket_state{}, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
