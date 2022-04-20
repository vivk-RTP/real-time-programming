%%%-------------------------------------------------------------------
%%% @author Volcov Oleg
%%% @copyright (C) 2022, FAF-191
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(worker_group_sup).

-behaviour(supervisor).

-export([start_link/1, init/1]).
-export([get_specs/2]).

start_link(WorkerSpec) ->
	supervisor:start_link(?MODULE, WorkerSpec).

init({ID, WorkerSpec}) ->
	io:format("[~p] worker_group_sup superviser's `init` is called.~n", [self()]),

	MaxRestarts = 5000,
	MaxSecondsBetweenRestarts = 10,
	SupFlags = #{
		strategy => one_for_one,
		intensity => MaxRestarts,
		period => MaxSecondsBetweenRestarts
	},

	WorkerSup = worker_sup:get_specs(ID, WorkerSpec),
	WorkerManager = worker_manager:get_specs(self(), ID),
	WorkerScaler = worker_scaler:get_specs(self(), ID),

	ChildSpecs = [
		WorkerSup,
		WorkerManager,
		WorkerScaler
	],

	{ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% External functions
%%%===================================================================

get_specs(ID, WorkerSpec) ->
	#{
		id => list_to_atom(ID++"_worker_group_sup"),
		start => {worker_group_sup, start_link, [{ID, WorkerSpec}]},
		restart => permanent,
		shutdown => infinity,
		type => supervisor,
		modules => [worker_group_sup]
	}.

