%%%-------------------------------------------------------------------
%%% @author Volcov Oleg
%%% @copyright (C) 2022, FAF-191
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(name_processing).

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
	#{<<"screen_name">> := ScreenName} = User,

	io:format("[~p] worker is processed data with `Screen Name`=`~s`.~n", [self(), ScreenName]).


