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

	EngageRation = engagement_ration_calculation(FavoriteCount, RetweetCount, FollowersCount),

	io:format("[~p] worker is processed data with `Engagement Ratio`=`~p`.~n", [self(), EngageRation]).

engagement_ration_calculation(_, _, FollowersCount) when FollowersCount  =:= 0 ->
	0;
engagement_ration_calculation(FavoriteCount, RetweetCount, FollowersCount) ->
	(FavoriteCount + RetweetCount) / FollowersCount.