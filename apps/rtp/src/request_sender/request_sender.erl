%%%-------------------------------------------------------------------
%%% @author Volcov Oleg
%%% @copyright (C) 2022, FAF-191
%%% @doc
%%% @end
%%%-------------------------------------------------------------------

-module(request_sender).

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
	error_logger:error_msg(["[~p] request_sender's `init` was crashed! "++
							"`Url`=`~p` param is not a list.~n"], [self(), Url]),
	{stop, error};
init(Url) when length(Url) =:= 0 ->
	error_logger:error_msg(["[~p] request_sender's `init` was crashed! "++
							"`Url`=`~p` param is empty.~n"], [self(), Url]),
	{stop, error};
init(Url) ->
	io:format("[~p] request_sender's `init` with `Url`=`~p` is called.~n", [self(), Url]),
	{ok, []}.

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast(_Request, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

%%%===================================================================
%%% External functions
%%%===================================================================

get_specs(Id, Url) ->
	#{
		id => Id,
		start => {request_sender, start_link, [Url]},
		restart => permanent,
		shutdown => infinity,
		type => worker,
		modules => [request_sender]
	}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
