%%%-------------------------------------------------------------------
%%% @author Volcov Oleg
%%% @copyright (C) 2022, FAF-191
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(hashtag_analyzer).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([get_specs/0]).

-define(INTERVAL, 1000).

-record(hashtag_analyzer_state, {hashtag_map}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	io:format("[~p] hashtag_analyzer's `init` with is called.~n", [self()]),

	NewState = #hashtag_analyzer_state{hashtag_map = #{}},

	erlang:send_after(?INTERVAL, self(), trigger),
	{ok, NewState}.

handle_call(_Request, _From, State = #hashtag_analyzer_state{}) ->
	{reply, ok, State}.

handle_cast({put, []}, State = #hashtag_analyzer_state{}) ->
	{noreply, State};
handle_cast({put, HashTags}, State = #hashtag_analyzer_state{hashtag_map = HashTagMap}) ->
	NewHashtagMap = process_hashtags(HashTags, HashTagMap),
	NewState = State#hashtag_analyzer_state{hashtag_map = NewHashtagMap},
	{noreply, NewState};
handle_cast(_Request, State = #hashtag_analyzer_state{}) ->
	{noreply, State}.

handle_info(trigger, State = #hashtag_analyzer_state{hashtag_map = HashTagMap}) ->
	HashTagList = maps:to_list(HashTagMap),
	HashTagSortedList = lists:keysort(2, HashTagList),
	{HashTag, Amount} = lists:last(HashTagSortedList),

	io:format("~n[~p] hashtag_analyzer's `raport` with most popular `Hashtag`=`~s` with `Amount`=~p is called.~n~n",
		[self(), HashTag, Amount]),

	erlang:send_after(?INTERVAL, self(), trigger),
	{noreply, State};
handle_info(_Info, State = #hashtag_analyzer_state{}) ->
	{noreply, State}.

%%%===================================================================
%%% External functions
%%%===================================================================

get_specs() ->
	#{
		id => hashtag_analyzer,
		start => {hashtag_analyzer, start_link, []},
		restart => permanent,
		shutdown => infinity,
		type => worker,
		modules => [hashtag_analyzer]
	}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

process_hashtags([], HashTagMap) ->
	HashTagMap;
process_hashtags([Head|Tail], HashTagMap) ->
	#{<<"text">> := HashTagText} = Head,
	Found = maps:find(HashTagText, HashTagMap),
	NewHashTagMap = increment_hashtag(Found, HashTagText, HashTagMap),
	process_hashtags(Tail, NewHashTagMap).

increment_hashtag({ok, Amount}, Hashtag, HashtagMap) ->
	HashtagMap#{Hashtag := Amount+1};
increment_hashtag(error, Hashtag, HashtagMap) ->
	HashtagMap#{Hashtag => 1}.


