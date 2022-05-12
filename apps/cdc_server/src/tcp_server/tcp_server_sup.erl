%%%-------------------------------------------------------------------
%%% @author Volcov Oleg
%%% @copyright (C) 2022, FAF-191
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(tcp_server_sup).

-behaviour(supervisor).

-export([start_link/1, init/1]).
-export([get_specs/1]).

start_link(Port) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, Port).

init(Port) ->
	io:format("[~p] tcp_server_sup superviser's `init` is called.~n", [self()]),

	MaxRestarts = 5000,
	MaxSecondsBetweenRestarts = 10,
	SupFlags = #{
		strategy => one_for_one,
		intensity => MaxRestarts,
		period => MaxSecondsBetweenRestarts
	},

	TCPSocketPoolSup = tcp_socket_pool_sup:get_specs(Port),
	TCPServerScaler = tcp_server_scaler:get_specs(),

	ChildSpecs = [
		TCPSocketPoolSup,
		TCPServerScaler
	],

	{ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% External functions
%%%===================================================================

get_specs(Port) ->
	#{
		id => tcp_server_sup,
		start => {tcp_server_sup, start_link, [Port]},
		restart => permanent,
		shutdown => infinity,
		type => supervisor,
		modules => [tcp_server_sup]
	}.

