%%%-------------------------------------------------------------------
%%% @author Volcov Oleg
%%% @copyright (C) 2022, FAF-191
%%% @doc Actor to receive json data from ‘Worker Manager’,
%%%         serialize it, sleep for a half of a second
%%%         to imitate some processing and print data in output.
%%% @end
%%%-------------------------------------------------------------------
-module(worker).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([get_specs/1]).

-define(HASHTAG_ANALYZER, hashtag_analyzer).

-record(worker_state, {work_func, last_tweet}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link(WorkFunc) ->
	gen_server:start_link(?MODULE, WorkFunc, []).

init(WorkFunc) ->
	State = #worker_state{work_func = WorkFunc},
	{ok, State}.

handle_call(_Request, _From, State = #worker_state{}) ->
	{reply, ok, State}.

handle_cast({tweet, JSONData}, State = #worker_state{work_func = WorkFunc}) ->
	MilliSeconds = rand:uniform(450) + 50,

	timer:sleep(MilliSeconds),

	Tweet = is_process_ready(JSONData),
	WorkFunc(Tweet),

	{noreply, State};
handle_cast(_Request, State = #worker_state{}) ->
	{noreply, State}.

handle_info(_Info, State = #worker_state{}) ->
	{noreply, State}.

%%%===================================================================
%%% External functions
%%%===================================================================

get_specs(WorkFunc) ->
	#{
		id => worker,
		start => {worker, start_link, [WorkFunc]},
		restart => permanent,
		shutdown => infinity,
		type => worker,
		modules => [worker]
	}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

is_process_ready(Data) when is_map(Data) =:= true ->
	process_tweet(Data);
is_process_ready(Data) when is_map(Data) =:= false ->
	BJSONData = list_to_binary(Data),
	IsJSON = jsx:is_json(BJSONData),

	tweet_parsing(BJSONData, IsJSON).

tweet_parsing(BTweet, IsJSON) when IsJSON =:= false ->
	Tweet = unicode:characters_to_list(BTweet, utf8),
	IsPanic = string:find(Tweet, "panic") =/= nomatch,
	process_error(IsPanic),
	exit(normal);
tweet_parsing(BTweet, IsJSON) when IsJSON =:= true ->
	JSON = jsx:decode(BTweet),
	#{<<"message">> := Message} = JSON,
	#{<<"tweet">> := Tweet} = Message,
	process_tweet(Tweet).

process_tweet(Tweet) ->
	#{<<"user">> := User} = Tweet,
	#{<<"screen_name">> := ScreenName} = User,

	#{<<"entities">> := Entities} = Tweet,
	#{<<"hashtags">> := HashTags} = Entities,

	gen_server:cast(?HASHTAG_ANALYZER, {put, HashTags}),

	io:format("[~p] worker is processed data with `Screen Name`=`~s`.~n", [self(), ScreenName]),

	Tweet.

process_error(IsPanic) when IsPanic =:= true ->
	error_logger:error_msg(create_error_message("Panic!"));
process_error(IsPanic) when IsPanic =:= false ->
	error_logger:error_msg(create_error_message("Wrong formatting!")).

create_error_message(Error) ->
	io_lib:format("[~p] worker is stopped with `Error`=`~s`.~n", [self(), Error]).