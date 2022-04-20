%%%-------------------------------------------------------------------
%%% @author Volcov Oleg
%%% @copyright (C) 2022, FAF-191
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(sink_metrics).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2]).
-export([get_specs/0]).

-define(INTERVAL, 3000).

-record(percentile_state, {list}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	gen_server:cast(message_broker, {subscribe, sink_metrics, self()}),
	erlang:send_after(?INTERVAL, self(), trigger),
	NewState = #percentile_state{list = []},
	{ok, NewState}.

handle_call(_Request, _From, State = #percentile_state{}) ->
	{reply, ok, State}.

handle_cast({sink_metrics, Data}, State = #percentile_state{list = List}) ->
    NewState = State#percentile_state{list = [Data|List]},
    {noreply, NewState};
handle_cast(_Request, State = #percentile_state{}) ->
	{noreply, State}.

handle_info(trigger, State = #percentile_state{list = List}) ->
	SortedList = lists:sort(List),
	ListSize = length(SortedList),

	P75th = calculate_percentile(ListSize, SortedList, 75),
	P90th = calculate_percentile(ListSize, SortedList, 90),
	P95th = calculate_percentile(ListSize, SortedList, 95),

	io:format("~n[~p] percentile's calculation of execution time for last ~p millis[in millis]:~n", [self(), ?INTERVAL]),
	io:format("  `75th`=~p,~n  `90th`=~p,~n  `95th`=~p.~n~n", [P75th, P90th, P95th]),

	erlang:send_after(?INTERVAL, self(), trigger),
	NewState = State#percentile_state{list = []},
	{noreply, NewState};
handle_info(_Info, State = #percentile_state{}) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	gen_server:cast(message_broker, {unsubscribe, sink_metrics, self()}),
	ok.

%%%===================================================================
%%% External functions
%%%===================================================================

get_specs() ->
	#{
		id => sink_metrics,
		start => {sink_metrics, start_link, []},
		restart => permanent,
		type => worker,
		modules => [sink_metrics]
	}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

calculate_percentile(ListSize, SortedList, Percentile) ->
	IndexFloat = math:ceil(Percentile / 100.0 * ListSize),
	Index = trunc(IndexFloat),

	lists:nth(Index - 1, SortedList).