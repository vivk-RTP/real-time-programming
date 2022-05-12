%%%-------------------------------------------------------------------
%%% @author Volcov Oleg
%%% @copyright (C) 2022, FAF-191
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(tcp_socket_pool_sup).

-behaviour(supervisor).

-export([start_link/1, init/1]).
-export([get_specs/1, start_worker/0]).

-define(OPTIONS, [{active, true}, {packet, 0}]).

start_link(Port) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, Port).

init(Port) ->
	MaxRestarts = 5000,
	MaxSecondsBetweenRestarts = 10,
	SupFlags = #{
		strategy => simple_one_for_one,
		intensity => MaxRestarts,
		period => MaxSecondsBetweenRestarts
	},

	io:format("[~p] tcp_socket_pool_sup is started!~n", [self()]),
	{ok, NewSocket} = gen_tcp:listen(Port, ?OPTIONS),
	io:format("[~p] tcp_socket_pool_sup server started with Socket=[~p]~n", [self(), NewSocket]),

	TCPSocket = tcp_socket:get_specs(NewSocket),

	Children = [
		TCPSocket
	],

	{ok, {SupFlags, Children}}.

%%%===================================================================
%%% External functions
%%%===================================================================

get_specs(Port) ->
	#{
		id => tcp_socket_pool_sup,
		start => {tcp_socket_pool_sup, start_link, [Port]},
		restart => permanent,
		shutdown => infinity,
		type => supervisor,
		modules => [tcp_socket_pool_sup]
	}.

start_worker() ->
	{ok, WorkerPID} = supervisor:start_child(?MODULE, []),
	WorkerPID.

%%%===================================================================
%%% Internal functions
%%%===================================================================
