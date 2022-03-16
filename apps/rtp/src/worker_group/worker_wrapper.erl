%%%-------------------------------------------------------------------
%%% @author Volcov Oleg
%%% @copyright (C) 2022, FAF-191
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(worker_wrapper).

-behaviour(supervisor).

-export([start_link/0, init/1]).
-export([send_message/2]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	MaxRestarts = 100,
	MaxSecondsBetweenRestarts = 10,
	SupFlags = #{
		strategy => all_for_one,
		intensity => MaxRestarts,
		period => MaxSecondsBetweenRestarts
	},

%%	Worker = worker:get_specs(),

	Children = [
%%		Worker
	],

	{ok, {SupFlags, Children}}.

%%%===================================================================
%%% External functions
%%%===================================================================

send_message(SelfPID, Message) ->
	Children = supervisor:which_children(SelfPID),
	send_to_children(Children, Message).

%%%===================================================================
%%% Internal functions
%%%===================================================================

send_to_children([], _) ->
	ok;
send_to_children([Head|Tail], Message) ->
	{_, Child, _, _} = Head,
	gen_server:cast(Child, {tweet, Message}),
	send_to_children(Tail, Message).

