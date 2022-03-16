%%%-------------------------------------------------------------------
%%% @author Volcov Oleg
%%% @copyright (C) 2022, FAF-191
%%% @doc
%%% @end
%%%-------------------------------------------------------------------

-module(retweet_analyzer).

-export([work_handler/0]).

-define(WORKER_MANAGER, worker_manager).

%%%===================================================================
%%% External functions
%%%===================================================================

work_handler() ->
	fun work/1.

%%%===================================================================
%%% Internal functions
%%%===================================================================

work(Tweet) ->
	#{<<"retweeted_status">> := Retweet} = Tweet,
	gen_server:cast(?WORKER_MANAGER, {tweet, Retweet}).