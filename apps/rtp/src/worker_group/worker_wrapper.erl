%%%-------------------------------------------------------------------
%%% @author Volcov Oleg
%%% @copyright (C) 2022, FAF-191
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(worker_wrapper).

-behaviour(supervisor).

-export([start_link/0, init/1]).
-export([send_message/2, get_specs/0]).

start_link() ->
	supervisor:start_link(?MODULE, []).

init([]) ->
	MaxRestarts = 0,
	MaxSecondsBetweenRestarts = 10,
	SupFlags = #{
		strategy => one_for_all,
		intensity => MaxRestarts,
		period => MaxSecondsBetweenRestarts
	},

	RetweetWorker = worker:get_specs("retweet", retweet_processing:work_handler()),
	HashTagWorker = worker:get_specs("hash_tag", hashtag_processing:work_handler()),
	NameWorker = worker:get_specs("name", name_processing:work_handler()),

	Children = [
		RetweetWorker,
		HashTagWorker,
		NameWorker
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

