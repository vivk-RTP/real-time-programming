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

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([get_specs/0]).

-record(worker_state, {amount}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
	gen_server:start_link(?MODULE, [], []).

init([]) ->
	{ok, #worker_state{}}.

handle_call({get_amount}, _From, State = #worker_state{}) ->
	%% TODO: return amount of messages in `mailbox`
	{reply, ok, State};
handle_call(_Request, _From, State = #worker_state{}) ->
	{reply, ok, State}.

handle_cast({add_tweet, _JSONData}, State = #worker_state{}) ->
	%%  TODO: Add tweet entry point
%%	gen_server:cast(self, {inc}),
%%	gen_server:cast(self, {tweet, JSONData}),
	{noreply, State};
handle_cast({inc}, State = #worker_state{}) ->
	%%  TODO: Increment amount of tweets
	{noreply, State};
handle_cast({dec}, State = #worker_state{}) ->
	%% TODO: Decrement amount of tweets
	{noreply, State};
handle_cast({tweet, JSONData}, State = #worker_state{}) ->
	MilliSeconds = rand:uniform(450) + 50,

	timer:sleep(MilliSeconds),

	BJSONData = list_to_binary(JSONData),
	IsJSON = jsx:is_json(BJSONData),

	process_tweet(BJSONData, IsJSON),
%%	gen_server:cast(self, {dec}),
	{noreply, State};
handle_cast(_Request, State = #worker_state{}) ->
	{noreply, State}.

handle_info(_Info, State = #worker_state{}) ->
	{noreply, State}.

%%%===================================================================
%%% External functions
%%%===================================================================

get_specs() ->
	#{
		id => worker,
		start => {worker, start_link, []},
		restart => permanent,
		shutdown => infinity,
		type => worker,
		modules => [worker]
	}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

process_tweet(BTweet, IsJSON) when IsJSON =:= false ->
	Tweet = unicode:characters_to_list(BTweet, utf8),
	IsPanic = string:find(Tweet, "panic") =/= nomatch,
	process_error(IsPanic),
	exit(normal);
process_tweet(BTweet, IsJSON) when IsJSON =:= true ->
	JSON = jsx:decode(BTweet),
	#{<<"message">> := Message} = JSON,
	#{<<"tweet">> := Tweet} = Message,
	#{<<"user">> := User} = Tweet,
	#{<<"screen_name">> := ScreenName} = User,

	io:format("[~p] worker is processed data with `Screen Name`=`~s`.~n", [self(), ScreenName]).

process_error(IsPanic) when IsPanic =:= true ->
	error_logger:error_msg(create_error_message("Panic!"));
process_error(IsPanic) when IsPanic =:= false ->
	error_logger:error_msg(create_error_message("Wrong formatting!")).

create_error_message(Error) ->
	io_lib:format("[~p] worker is stopped with `Error`=`~s`.~n", [self(), Error]).