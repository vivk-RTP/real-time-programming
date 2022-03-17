%%%-------------------------------------------------------------------
%%% @author Voclov Oleg
%%% @copyright (C) 2022, FAF-191
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(sentiment_score_processing).

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
	#{<<"text">> := Text} = Tweet,

	AverageScore = sentimental_score_calculation(Text),

	io:format("[~p] worker is processed data with `Score`=`~p`.~n", [self(), AverageScore]).

iterate_score([], Score, Iterator) ->
	{Score, Iterator};
iterate_score([Head|Tail], Score, Iterator) ->
	WordScore = emotion_analyzer:get_value(Head),
	iterate_score(Tail, Score+WordScore, Iterator+1).

sentimental_score_calculation(Text) ->
	Words = re:split(Text, "[ .,?!:-;/'()@]"),
	{Score, Count} = iterate_score(Words, 0, 0),

	Score / Count.
