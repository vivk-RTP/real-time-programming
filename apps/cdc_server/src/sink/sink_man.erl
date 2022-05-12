%%%-------------------------------------------------------------------
%%% @author Volcov Oleg
%%% @copyright (C) 2022, FAF-191
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(sink_man).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([get_specs/0]).

-record(sink_man_state, {}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	NewState = #sink_man_state{},
	{ok, NewState}.

handle_call(_Request, _From, State = #sink_man_state{}) ->
	{reply, ok, State}.

handle_cast({Attribute, Message}, State = #sink_man_state{}) ->
	gen_server:cast(sink, {put, Attribute, Message}),
	{noreply, State};
handle_cast(_Request, State = #sink_man_state{}) ->
	{noreply, State}.

handle_info(_Info, State = #sink_man_state{}) ->
	{noreply, State}.

%%%===================================================================
%%% External functions
%%%===================================================================

get_specs() ->
	#{
		id => sink_man,
		start => {sink_man, start_link, []},
		restart => permanent,
		shutdown => infinity,
		type => worker,
		modules => [sink_man]
	}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
