%%%-------------------------------------------------------------------
%%% @author Volcov Oleg
%%% @copyright (C) 2022, FAF-191
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(aggregator).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([get_specs/0]).

-define(SERVER, ?MODULE).

-record(aggregator_state, {map}).
-record(data, {is_tweet, tweet, is_engage_ratio, engage_ratio, is_sent_score, sent_score}).


%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
	NewState = #aggregator_state{map = #{}},
	{ok, NewState}.

handle_call(_Request, _From, State = #aggregator_state{}) ->
	{reply, ok, State}.

handle_cast({tweet, ID, Tweet}, State = #aggregator_state{map = Map}) ->
	Data = get_by_id(ID, Map),
	NewData = Data#data{is_tweet = true, tweet = Tweet},
	NewState = update_state(ID, NewData, State),
	{noreply, NewState};
handle_cast({sent_score, ID, SentScore}, State = #aggregator_state{map = Map}) ->
	Data = get_by_id(ID, Map),
	NewData = Data#data{is_sent_score = true, sent_score = SentScore},
	NewState = update_state(ID, NewData, State),
	{noreply, NewState};
handle_cast({engage_ratio, ID, EngageRatio}, State = #aggregator_state{map = Map}) ->
	Data = get_by_id(ID, Map),
	NewData = Data#data{is_engage_ratio = true, engage_ratio = EngageRatio},
	NewState = update_state(ID, NewData, State),
	{noreply, NewState};
handle_cast(_Request, State = #aggregator_state{}) ->
	{noreply, State}.

handle_info(_Info, State = #aggregator_state{}) ->
	{noreply, State}.

%%%===================================================================
%%% External functions
%%%===================================================================

get_specs() ->
	#{
		id => aggregator,
		start => {aggregator, start_link, []},
		restart => permanent,
		shutdown => infinity,
		type => worker,
		modules => [aggregator]
	}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

update_state(ID, Data, State = #aggregator_state{map = Map}) ->
	IsFull = is_full(Data),
	NewMap = update_map(IsFull, ID, Data, Map),
	State#aggregator_state{map = NewMap}.

convert_data(_ = #data{tweet = Tweet, engage_ratio = ER, sent_score = SS}) ->
	#{<<"user">> := User} = Tweet,
	UserlessTweet = maps:remove(<<"user">>, Tweet),

	FinalTweet = UserlessTweet#{
		<<"engagement_ratio">> => ER,
		<<"sentiment_score">> => SS
	},

	{User, FinalTweet}.

send_data(Data = #data{sent_score = SS, engage_ratio = ER}) ->
	{User, Tweet} = convert_data(Data),
	%% TODO: Send full data to sink
	io:format("[~p] aggregator's full data with User = [~n~p~n] and Tweet = [~n~p~n]~n", [self(), User, Tweet]),
	ok.

update_map(true, ID, Data, Map) ->
	send_data(Data),
	maps:remove(ID, Map);
update_map(false, ID, Data, Map) ->
	Map#{ID => Data}.

is_full(_ = #data{is_tweet = IsTweet, is_engage_ratio = IsER, is_sent_score = IsSS}) ->
	IsTweet and IsER and IsSS.

create_is_empty({ok, Element}) ->
	Element;
create_is_empty(error) ->
	#data{is_tweet = false, is_sent_score = false, is_engage_ratio = false}.

get_by_id(ID, Map) ->
	Element = maps:find(ID, Map),
	create_is_empty(Element).


