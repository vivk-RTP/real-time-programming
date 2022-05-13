%%%-------------------------------------------------------------------
%%% @author Volcov Oleg
%%% @copyright (C) 2022, FAF-191
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(attribute_man).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([get_specs/0]).

-record(attribute_man_state, {attributes}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	NewState = #attribute_man_state{attributes = #{}},
	{ok, NewState}.

handle_call(_Request, _From, State = #attribute_man_state{}) ->
	{reply, ok, State}.

handle_cast({Attribute, CMD, Param}, State = #attribute_man_state{}) ->
	{AttributePID, NewState} = find_attribute_worker(Attribute, State),
	gen_server:cast(AttributePID, {CMD, Param}),
	{noreply, NewState};
handle_cast(_Request, State = #attribute_man_state{}) ->
	{noreply, State}.

handle_info(_Info, State = #attribute_man_state{}) ->
	{noreply, State}.

%%%===================================================================
%%% External functions
%%%===================================================================

get_specs() ->
	#{
		id => attribute_man,
		start => {attribute_man, start_link, []},
		restart => permanent,
		shutdown => infinity,
		type => worker,
		modules => [attribute_man]
	}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

find_attribute_worker(Attribute, State = #attribute_man_state{attributes = Attributes}) ->
	Found = maps:find(Attribute, Attributes),
	get_attribute_worker(Found, Attribute, State).

get_attribute_worker({ok, PID}, _Attribute, State = #attribute_man_state{attributes = _Attributes}) ->
	{PID, State};
get_attribute_worker(error, Attribute, State = #attribute_man_state{attributes = Attributes}) ->
	AttributePID = attribute_pool:start_attribute(Attribute),
	NewAttributes = Attributes#{Attribute => AttributePID},
	NewState = State#attribute_man_state{attributes = NewAttributes},
	{AttributePID, NewState}.