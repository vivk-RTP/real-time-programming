%%%-------------------------------------------------------------------
%%% @author Voclov Oleg
%%% @copyright (C) 2022, FAF-191
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(hashtag_processing).

-export([work_handler/0]).

-define(HASHTAG_ANALYZER, hashtag_analyzer).

%%%===================================================================
%%% External functions
%%%===================================================================

work_handler() ->
	fun work/1.

%%%===================================================================
%%% Internal functions
%%%===================================================================

work(Tweet) ->
	#{<<"entities">> := Entities} = Tweet,
	#{<<"hashtags">> := HashTags} = Entities,

	gen_server:cast(?HASHTAG_ANALYZER, {put, HashTags}).

