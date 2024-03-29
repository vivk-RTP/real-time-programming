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
	NameWorker = worker:get_specs("name", tweet_processing:work_handler()),
	EngageRationWorker = worker:get_specs("engage_ration", engagement_ratio_processing:work_handler()),
	SentimentScoreWorker = worker:get_specs("sentiment_score", sentiment_score_processing:work_handler()),

	Children = [
		RetweetWorker,
		HashTagWorker,
		NameWorker,
		EngageRationWorker,
		SentimentScoreWorker
	],

	{ok, {SupFlags, Children}}.

%%%===================================================================
%%% External functions
%%%===================================================================

get_specs() ->
	#{
		id => worker_wrapper,
		start => {worker_wrapper, start_link, []},
		restart => permanent,
		shutdown => infinity,
		type => supervisor,
		modules => [worker_wrapper]
	}.

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

