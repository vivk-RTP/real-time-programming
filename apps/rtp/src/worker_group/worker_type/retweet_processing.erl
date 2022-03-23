%%%-------------------------------------------------------------------
%%% @author Volcov Oleg
%%% @copyright (C) 2022, FAF-191
%%% @doc
%%% @end
%%%-------------------------------------------------------------------

-module(retweet_processing).

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

send(Retweeted, _) when Retweeted =:= false ->
	io:format("[~p] Retweeted=false!!~n", [self()]),
	ok;
send(_, Tweet) ->
	#{<<"retweeted_status">> := Retweet} = Tweet,
	io:format("[~p] Retweeted=true!!!!n", [self()]),
	gen_server:cast(?WORKER_MANAGER, {tweet, Retweet}).

work(Tweet) ->
	Retweeted = is_map_key(<<"retweeted_status">>, Tweet),
	send(Retweeted, Tweet).