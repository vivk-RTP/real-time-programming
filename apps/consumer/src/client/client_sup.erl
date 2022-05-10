%%%-------------------------------------------------------------------
%%% @author Volcov Oleg
%%% @copyright (C) 2022, FAF-191
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(client_sup).

-behaviour(supervisor).

-export([start_link/0, init/1, get_specs/0]).

-define(USERS_ATTRIBUTE, "Users").
-define(TWEETS_ATTRIBUTE, "Tweets").

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	io:format("[~p] client superviser's `init` is called.~n", [self()]),

	MaxRestarts = 100,
	MaxSecondsBetweenRestarts = 10,
	SupFlags = #{
		strategy => one_for_all,
		intensity => MaxRestarts,
		period => MaxSecondsBetweenRestarts
	},

	TCPClient = tcp_subscriber:get_specs(),
	Client = client:get_specs(?TWEETS_ATTRIBUTE),

	Children = [
		TCPClient,
		Client
	],

	{ok, {SupFlags, Children}}.

%%%===================================================================
%%% External functions
%%%===================================================================

get_specs() ->
	#{
		id => client_sup,
		start => {client_sup, start_link, []},
		restart => permanent,
		shutdown => infinity,
		type => supervisor,
		modules => [client_sup]
	}.
