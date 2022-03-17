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
-export([get_specs/2]).

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

handle_cast({tweet, LTweet}, State = #worker_state{work_func = WorkFunc}) ->
	MilliSeconds = rand:uniform(450) + 50,

	timer:sleep(MilliSeconds),

	Tweet = get_tweet_map(LTweet),
	WorkFunc(Tweet),

	{noreply, State};
handle_cast(_Request, State = #worker_state{}) ->
	{noreply, State}.

handle_info(_Info, State = #worker_state{}) ->
	{noreply, State}.

%%%===================================================================
%%% External functions
%%%===================================================================

get_specs(Name, WorkFunc) ->
	#{
		id => Name++"_worker",
		start => {worker, start_link, [WorkFunc]},
		restart => permanent,
		shutdown => infinity,
		type => worker,
		modules => [worker]
	}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_tweet_map(Map) when is_map(Map) =:= true ->
	Map;
get_tweet_map(LTweet) when is_map(LTweet) =:= false ->
	BTweet = list_to_binary(LTweet),
	IsJson = jsx:is_json(BTweet),

	tweet_parsing(BTweet, IsJson).

tweet_parsing(BTweet, IsJson) when IsJson =:= false ->
	Tweet = unicode:characters_to_list(BTweet, utf8),
	IsPanic = string:find(Tweet, "panic") =/= nomatch,
	process_error(IsPanic),
	exit(normal);
tweet_parsing(BTweet, IsJson) when IsJson =:= true ->
	Map = jsx:decode(BTweet),
	#{<<"message">> := Message} = Map,
	#{<<"tweet">> := Tweet} = Message,
	Tweet.

process_error(IsPanic) when IsPanic =:= true ->
	error_logger:error_msg(create_error_message("Panic!"));
process_error(IsPanic) when IsPanic =:= false ->
	error_logger:error_msg(create_error_message("Wrong formatting!")).

create_error_message(Error) ->
	io_lib:format("[~p] worker is stopped with `Error`=`~s`.~n", [self(), Error]).