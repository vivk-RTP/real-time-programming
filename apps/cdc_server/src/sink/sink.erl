%%%-------------------------------------------------------------------
%%% @author Volcov Oleg
%%% @copyright (C) 2022, FAF-191
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(sink).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([get_specs/0]).

-define(DB_NAME, <<"tweet_analyzer">>).
-define(USER_COLLECTION, <<"Users">>).
-define(TWEET_COLLECTION, <<"Tweets">>).

-define(BATCH_SIZE, 32).

-define(INTERVAL, 1000).

-record(sink_state, {length, list, connection, timer}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	application:ensure_all_started(mongodb),
	InitState = get_empty_state(),
	TimerRef = create_timer(),
	TimerState = InitState#sink_state{timer = TimerRef},
	ConnectionStatus = mc_worker_api:connect([{database, ?DB_NAME}, {host, "mongo"}]),
	NewState = analyze_connection(ConnectionStatus, TimerState),
	io:format("[~p] sink's init call with new state = [~p].~n", [self(), NewState]),
	{ok, NewState}.

handle_call(_Request, _From, State = #sink_state{}) ->
	{reply, ok, State}.

handle_cast({put, Attribute, Message}, State = #sink_state{}) ->
	UpdateState = update_state({data, Attribute, Message}, State),
	AfterBatchState = batch_check(UpdateState),
	{noreply, AfterBatchState};
handle_cast(_Request, State = #sink_state{}) ->
	{noreply, State}.

handle_info(trigger, State = #sink_state{}) ->
	NewState = insert_database(State),
	{noreply, NewState};
handle_info(_Info, State = #sink_state{}) ->
	{noreply, State}.

%%%===================================================================
%%% External functions
%%%===================================================================

get_specs() ->
	#{
		id => sink,
		start => {sink, start_link, []},
		restart => permanent,
		shutdown => infinity,
		type => worker,
		modules => [sink]
	}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

analyze_connection({error, Reason}, _) ->
	Error = io_lib:format("[~p] sink is stopped with `Reason`=`~s`.~n", [self(), Reason]),
	error_logger:error_msg(Error),
	exit(normal);
analyze_connection({ok, Connection}, State) ->
	NewState = State#sink_state{connection = Connection},
	NewState.

update_state(NewData, State = #sink_state{length = Length, list = List}) ->
	NewList = [NewData|List],
	NewLength = Length+1,
	NewState = State#sink_state{length = NewLength, list = NewList},
	NewState.

get_empty_state() ->
	get_empty_state(#sink_state{}).

get_empty_state(State = #sink_state{connection = Connection}) ->
	NewState = State#sink_state{length = 0, list = [], connection = Connection},
	NewState.

send_data([], _Connection) ->
	ok;
send_data([{data, Attribute, Message}|Tail], Connection) ->
	mc_worker_api:insert(Connection, Attribute, Message),
	tcp_client:publish(Attribute, jsx:encode(Message)),

	send_data(Tail, Connection).

insert_database(State = #sink_state{length = Length, list = List, connection = Connection}) ->
	send_data(List, Connection),

%%	io:format("~n[~p] sink's database insertion with length = ~p~n~n", [self(), Length]),
	TimerState = update_timer(State),

	NewState = get_empty_state(TimerState),
	NewState.

batch_check(State = #sink_state{length = Length}) when Length >= ?BATCH_SIZE ->
	NewState = insert_database(State),
	NewState;
batch_check(State = #sink_state{}) ->
	State.

create_timer() ->
	erlang:send_after(?INTERVAL, self(), trigger).

update_timer(State = #sink_state{timer = TimerRef}) ->
	TimerResult = erlang:cancel_timer(TimerRef),
	publish_timer_metrics(TimerResult),
	NewTimerRef = create_timer(),
	NewState = State#sink_state{timer = NewTimerRef},
	NewState.

publish_timer_metrics(false) ->
	gen_server:cast(message_broker, {publish, sink_metrics, ?INTERVAL});
publish_timer_metrics(TimerResult) ->
	gen_server:cast(message_broker, {publish, sink_metrics, ?INTERVAL - TimerResult}).


