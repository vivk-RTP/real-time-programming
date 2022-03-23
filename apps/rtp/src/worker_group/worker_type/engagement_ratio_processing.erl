%%%-------------------------------------------------------------------
%%% @author Volcov Oleg
%%% @copyright (C) 2022, FAF-191
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(engagement_ratio_processing).

-export([]).

-export([work_handler/0]).

%%%===================================================================
%%% External functions
%%%===================================================================

work_handler() ->
	fun work/1.

%%%===================================================================
%%% Internal functions
%%%===================================================================

work(Tweet) ->
	#{<<"user">> := User} = Tweet,
	#{<<"followers_count">> := FollowersCount} = User,

	#{<<"favorite_count">> := FavoriteCount} = Tweet,
	#{<<"retweet_count">> := RetweetCount} = Tweet,

	#{<<"id">> := ID} = Tweet,

	EngageRation = engagement_ration_calculation(FavoriteCount, RetweetCount, FollowersCount),

	gen_server:cast(aggregator, {engage_ratio, ID, EngageRation}).

engagement_ration_calculation(_, _, FollowersCount) when FollowersCount  =:= 0 ->
	0;
engagement_ration_calculation(FavoriteCount, RetweetCount, FollowersCount) ->
	(FavoriteCount + RetweetCount) / FollowersCount.