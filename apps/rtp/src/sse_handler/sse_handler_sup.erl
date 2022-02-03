%%%-------------------------------------------------------------------
%%% @author Volcov Oleg
%%% @copyright (C) 2022, FAF-191
%%% @doc Supervisor for multiple @Urls handle parallel.
%%% @end
%%%-------------------------------------------------------------------

-module(sse_handler_sup).

-behaviour(supervisor).

-export([start_link/1, init/1]).
-export([get_specs/1]).

start_link(Urls) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, Urls).

init(Urls) ->
	init(Urls, 0, []).

init([Head|Tails], _Id, Children) ->
	AChild = sse_handler:get_specs(_Id, Head),
	init(Tails, _Id+1, [AChild|Children]);
init([], _Id, Children) ->
	MaxRestarts = 100,
	MaxSecondsBetweenRestarts = 10,
	SupFlags = #{
		strategy => one_for_one,
		intensity => MaxRestarts,
		period => MaxSecondsBetweenRestarts
	},

	{ok, {SupFlags, Children}}.

%%%===================================================================
%%% External functions
%%%===================================================================

get_specs(Urls) ->
	#{
		id => sse_handler_sup,
		start => {sse_handler_sup, start_link, [Urls]},
		restart => permanent,
		shutdown => infinity,
		type => supervisor,
		modules => [sse_handler_sup]
	}.
