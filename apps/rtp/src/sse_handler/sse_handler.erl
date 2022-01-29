%%%-------------------------------------------------------------------
%%% @author Volcov Oleg
%%% @copyright (C) 2022, FAF-191
%%% @doc
%%% @end
%%%-------------------------------------------------------------------

-module(sse_handler).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([get_specs/2]).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link(Url) ->
	gen_server:start_link(?MODULE, Url, []).

init(Url) when is_list(Url) =:= false ->
	error_logger:error_msg(["[~p] sse_handler's `init` was crashed! "++
		"`Url`=`~p` param is not a list.~n"], [self(), Url]),
	{stop, error};
init(Url) when length(Url) =:= 0 ->
	error_logger:error_msg(["[~p] sse_handler's `init` was crashed! "++
		"`Url`=`~p` param is empty.~n"], [self(), Url]),
	{stop, error};
init(Url) ->
	io:format("[~p] sse_handler's `init` with `Url`=`~p` is called.~n", [self(), Url]),
	httpc:request(get, {Url, []}, [], [{sync, false}, {stream, self}]),
	{ok, []}.

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast(_Request, State) ->
	{noreply, State}.

handle_info({http, {_RequestId, stream_start, _Headers}}, State) ->
	io:format("[~p] sse_handler's `stream start` with `headers`=~p.~n", [self(), _Headers]),
	{noreply, State};
handle_info({http, {_RequestId, {error, _Reason}}}, State) ->
	Error = io_lib:format("[~p] sse_handler's `error` with `reason`=`~p`.~n", [self(), _Reason]),
	error_logger:error_msg(Error),
	{stop, _Reason, State};
handle_info({http, {_RequestId, stream_end, _Headers}}, State) ->
	Error = io_lib:format("[~p] sse_handler's `stream end` with `headers`=~p.~n", [self(), _Headers]),
	error_logger:error_msg(Error),
	{stop, normal, State};
handle_info({http, {_RequestId, stream, _Data}}, State) ->
%%	io:format("Info Data ~p~n",[_Data]),
	{noreply, State};
handle_info(_Info, State) ->
	io:format("~p~n", [_Info]),
	{noreply, State}.

%%%===================================================================
%%% External functions
%%%===================================================================

get_specs(Id, Url) ->
	#{
		id => integer_to_list(Id)++"_sse_handler",
		start => {sse_handler, start_link, [Url]},
		restart => transient,
		shutdown => infinity,
		type => worker,
		modules => [sse_handler]
	}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
