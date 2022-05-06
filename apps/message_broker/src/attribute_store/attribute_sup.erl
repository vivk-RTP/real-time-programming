%%%-------------------------------------------------------------------
%%% @author Volcov Oleg
%%% @copyright (C) 2022, FAF-191
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(attribute_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).
-export([get_specs/0]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	io:format("[~p] tcp_server_sup superviser's `init` is called.~n", [self()]),

	MaxRestarts = 5000,
	MaxSecondsBetweenRestarts = 10,
	SupFlags = #{
		strategy => one_for_one,
		intensity => MaxRestarts,
		period => MaxSecondsBetweenRestarts
	},

	AttributePool = attribute_pool:get_specs(),
	AttributeManager = attribute_man:get_specs(),

	ChildSpecs = [
		AttributePool,
		AttributeManager
	],

	{ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% External functions
%%%===================================================================

get_specs() ->
	#{
		id => attribute_sup,
		start => {attribute_sup, start_link, []},
		restart => permanent,
		shutdown => infinity,
		type => supervisor,
		modules => [attribute_sup]
	}.


