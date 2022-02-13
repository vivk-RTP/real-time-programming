%%%-------------------------------------------------------------------
%%% @author Volcov Oleg
%%% @copyright (C) 2022, FAF-191
%%% @doc Simple `worker` which will do some `Tweet` processing.
%%% @end
%%%-------------------------------------------------------------------
-module(worker).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([get_specs/0]).

-record(worker_state, {last_tweet}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
	gen_server:start_link(?MODULE, [], []).

init([]) ->
	{ok, #worker_state{}}.

handle_call(_Request, _From, State = #worker_state{}) ->
	{reply, ok, State}.

handle_cast({tweet, JSONData}, State = #worker_state{}) ->
	BJSONData = list_to_binary(JSONData),

	JSON = jsx:decode(BJSONData),
	#{<<"message">> := Message} = JSON,
	#{<<"tweet">> := Tweet} = Message,
	#{<<"user">> := User} = Tweet,
	#{<<"screen_name">> := ScreenName} = User,

	io:format("[~p] worker is processed data with `Screen Name`=`~s`.~n", [self(), ScreenName]),

	timer:sleep(10),
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
