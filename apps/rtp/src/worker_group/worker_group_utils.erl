%%%-------------------------------------------------------------------
%%% @author Volcov Oleg
%%% @copyright (C) 2022, FAF-191
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(worker_group_utils).

-export([get_specs/2]).

%%%===================================================================
%%% External functions
%%%===================================================================

get_specs(ID, WorkerHandler) ->
	Worker = worker:get_specs(ID, WorkerHandler),
	WorkerGroupSup = worker_group_sup:get_specs(ID, Worker),
	WorkerGroupSup.
