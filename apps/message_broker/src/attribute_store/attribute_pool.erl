%%%-------------------------------------------------------------------
%%% @author Volcov Oleg
%%% @copyright (C) 2022, FAF-191
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(attribute_pool).

-behaviour(supervisor).

-export([start_link/0, init/1]).
-export([get_specs/0, start_attribute/0]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	MaxRestarts = 5000,
	MaxSecondsBetweenRestarts = 10,
	SupFlags = #{
		strategy => simple_one_for_one,
		intensity => MaxRestarts,
		period => MaxSecondsBetweenRestarts
	},

	io:format("[~p] attribute_pool is started!~n", [self()]),

	Attribute = attribute:get_specs(),

	Children = [
		Attribute
	],

	{ok, {SupFlags, Children}}.

%%%===================================================================
%%% External functions
%%%===================================================================

get_specs() ->
	#{
		id => attribute_pool,
		start => {attribute_pool, start_link, []},
		restart => permanent,
		shutdown => infinity,
		type => supervisor,
		modules => [attribute_pool]
	}.

start_attribute() ->
	{ok, WorkerPID} = supervisor:start_child(attribute_pool, []),
	WorkerPID.
