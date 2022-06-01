%%%-------------------------------------------------------------------
%%% @author Volcov Oleg
%%% @copyright (C) 2022, FAF-191
%%% @doc Tweet-Analyzer main supervisor.
%%% @end
%%%-------------------------------------------------------------------

-module(rtp_sup).
-author("Volcov Oleg").

-behaviour(supervisor).

-define(TWEET_1, "http://rtp_source:4000/tweets/1").
-define(TWEET_2, "http://rtp_source:4000/tweets/2").

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    io:format("[~p] Tweet-Analyzer' superviser's `init` is called.~n", [self()]),

    MaxRestarts = 100,
    MaxSecondsBetweenRestarts = 10,
    SupFlags = #{
        strategy => one_for_one,
        intensity => MaxRestarts,
        period => MaxSecondsBetweenRestarts
    },

    SSEHandlerSup = sse_handler_sup:get_specs([?TWEET_1, ?TWEET_2]),
    HashTagAnalyzer = hashtag_analyzer:get_specs(),
    Aggregator = aggregator:get_specs(),
    Sink = sink:get_specs(),
    MessageBroker = message_broker:get_specs(),
    SinkMetrics = sink_metrics:get_specs(),
    TCPClient = tcp_client:get_specs(),

    RetweetGroupSup = worker_group_utils:get_specs("retweet", retweet_processing:work_handler()),
    HashTagGroupSup = worker_group_utils:get_specs("hash_tag", hashtag_processing:work_handler()),
    TweetGroupSup = worker_group_utils:get_specs("tweet", tweet_processing:work_handler()),
    EngageRatioGroupSup = worker_group_utils:get_specs("engage_ratio", engagement_ratio_processing:work_handler()),
    SentimentScoreGroupSup = worker_group_utils:get_specs("sentiment_score", sentiment_score_processing:work_handler()),

    ChildSpecs = [
        MessageBroker,
        HashTagAnalyzer,
        SSEHandlerSup,
        Aggregator,
        Sink,
        SinkMetrics,
        TCPClient,

        RetweetGroupSup,
        HashTagGroupSup,
        TweetGroupSup,
        EngageRatioGroupSup,
        SentimentScoreGroupSup
    ],

    {ok, {SupFlags, ChildSpecs}}.
