%%%-------------------------------------------------------------------
%%% @author Volcov Oleg
%%% @copyright (C) 2022, FAF-191
%%% @doc Add-On for `sse_handler` for message handling implementation.
%%% @end
%%%-------------------------------------------------------------------

-module(tweets).

-define(MESSAGE_START, "event: \"message\"\n\ndata: ").
-define(MESSAGE_END, "\n\n").
-define(EMPTY_LIST, []).

-export([process/2]).

%%--------------------------------------------------------------------
%% @doc Collect @BMessage data from stream, save in @State,
%%      remove useless data from stream,
%%      divide into Tweets and send it for future processing.
%%
%%      Return data `rest` in @NewState.
%% @end
%%--------------------------------------------------------------------

process(BMessage, State) ->
	LMessage = binary_to_list(BMessage),
	LMessageData = strings:replace(LMessage, ?MESSAGE_START, ?EMPTY_LIST),
	LTweets = string:split(State++LMessageData, ?MESSAGE_END),
	NewState = divide_tweets(LTweets, fun(Message) -> send_message(Message) end),
	NewState.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Divide messages into tweets and send it
%%      by @_SendMessageFunc function.
%%
%%      Return @Head as data `rest`.
%% @end
%%--------------------------------------------------------------------

divide_tweets([Head|[]], _SendMessageFunc) ->
	Head;
divide_tweets([Head|Tails], _SendMessageFunc) ->
	_SendMessageFunc(Head),
	divide_tweets(Tails, _SendMessageFunc).

%%--------------------------------------------------------------------
%% @doc Send tweet data in @Message for future processing.
%% @end
%%--------------------------------------------------------------------

send_message(Message) ->
	io:format("~n~n~n[~p][WIP] will send tweet to some worker :~n~s~n", [self(), Message]).
