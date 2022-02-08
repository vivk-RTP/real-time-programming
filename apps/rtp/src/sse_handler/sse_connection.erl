%%%-------------------------------------------------------------------
%%% @author Volcov Oleg
%%% @copyright (C) 2022, FAF-191
%%% @doc
%%% @end
%%%-------------------------------------------------------------------

-module(sse_connection).

%% API
-export([new/2]).

%%--------------------------------------------------------------------
%% @doc Sends an HTTP GET request on some @Url
%% 		and send result in @StreamTo actor's mailbox.
%%
%% 		Great for SSE connection.
%% @end
%%--------------------------------------------------------------------

new(Url, StreamTo) ->
	io:format("[~p] sse_connection send request to establish SSE connection on `Url`=`~p`.~n", [self(), Url]),
	httpc:request(get, {Url, []}, [], [{sync, false}, {stream, StreamTo}]).
