%%%-------------------------------------------------------------------
%%% @author Volcov Oleg
%%% @copyright (C) 2022, FAF-191
%%% @doc
%%% @end
%%%-------------------------------------------------------------------

-module(worker_group_sup).

-behaviour(supervisor).

-export([start_link/0, init/1, get_specs/0]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	MaxRestarts = 100,
	MaxSecondsBetweenRestarts = 10,
	SupFlags = #{
		strategy => one_for_one,
		intensity => MaxRestarts,
		period => MaxSecondsBetweenRestarts
	},

	Children = [],

	{ok, {SupFlags, Children}}.

%%%===================================================================
%%% External functions
%%%===================================================================

get_specs() ->
	#{
		id => worker_group_sup,
		start => {worker_group_sup, start_link, []},
		restart => permanent,
		shutdown => infinity,
		type => supervisor,
		modules => [worker_group_sup]
	}.
