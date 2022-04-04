%%%-------------------------------------------------------------------
%%% @author Volcov Oleg
%%% @copyright (C) 2022, FAF-191
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(worker_group_utils).

-export([get_specs/2, get_manager/1, get_scaler/1, get_worker_sup/1]).

%%%===================================================================
%%% External functions
%%%===================================================================

get_specs(ID, WorkerHandler) ->
	Worker = worker:get_specs(ID, WorkerHandler),
	WorkerGroupSup = worker_group_sup:get_specs(ID, Worker),
	WorkerGroupSup.

get_manager(SelfPID) ->
	AllChildren = supervisor:which_children(SelfPID),
	[Child|_] = get_child_by_modules(AllChildren, [], worker_manager),
	Child.

get_scaler(SelfPID) ->
	AllChildren = supervisor:which_children(SelfPID),
	[Child|_] = get_child_by_modules(AllChildren, [], worker_scaler),
	Child.

get_worker_sup(SelfPID) ->
	AllChildren = supervisor:which_children(SelfPID),
	[Child|_] = get_child_by_modules(AllChildren, [], worker_sup),
	Child.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_child_by_modules([], Acc, _) ->
	Acc;
get_child_by_modules([Head|Tail], Acc, Module) ->
	{_, PID, _, Modules} = Head,
	IsMember = lists:member(Module, Modules),
	NewAcc = is_member(IsMember, PID, Acc),
	get_child_by_modules(Tail, NewAcc, Module).


is_member(true, PID, Acc) ->
	[PID|Acc];
is_member(false, _, Acc) ->
	Acc.
