%%%-------------------------------------------------------------------
%%% @author Volcov Oleg
%%% @copyright (C) 2022, FAF-191
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(tcp_message_process_utils).

-export([process_tcp_data/3]).

process_tcp_data(LMessage, Stash, PID) ->
	NewMessage = Stash++LMessage,
	{_, IsJson} = is_json(NewMessage),
	return_stash(NewMessage, PID, IsJson).

is_json(LMessage) ->
	BMessage = list_to_binary(LMessage),
	IsJson = jsx:is_json(BMessage),
	{BMessage, IsJson}.

return_stash(Message, _, false) ->
	Message;
return_stash(Message, PID, true) ->
	ParseResult = parse_message(Message, true),
	{Attribute, CMD, Param} = analyze_command(ParseResult, PID),

	io:format("[~p] process Attribute=[~p].~n", [self(), Attribute]),
	gen_server:cast(attribute_man, {Attribute, CMD, Param}),
	[].

parse_message(_BMessage, false) ->
	ErrorMessage = io_lib:format("[~p] PARSE ERROR! IS NOT JSON!~n", [self()]),
	error_logger:error_msg(ErrorMessage),
	exit(normal);
parse_message(LMessage, true) ->
	io:format("[~p] process Message=[~s].~n", [self(), LMessage]),
	BMessage = list_to_binary(LMessage),
	Map = jsx:decode(BMessage),
	#{<<"topic">> := Attribute} = Map,
	#{<<"command">> := CMD} = Map,
	#{<<"param">> := Param} = Map,

	LAttribute = get_attribute(Attribute),

	{LAttribute, CMD, Param}.

get_attribute(BAttribute) when is_binary(BAttribute) ->
	LAttribute = binary_to_list(BAttribute),
	LAttribute;
get_attribute(LAttribute) ->
	LAttribute.

analyze_command({Attribute, CMD, _}, PID) when CMD =:= "subscribe" ->
	io:format("[~p] socket subscribed to Attribute=[~p].~n", [self(), Attribute]),
	{Attribute, subscribe, PID};
analyze_command({Attribute, CMD, _}, PID) when CMD =:= "unsubscribe" ->
	io:format("[~p] socket unsubscribed to Attribute=[~p].~n", [self(), Attribute]),
	{Attribute, unsubscribe, PID};
analyze_command({Attribute, CMD, Message}, _) when CMD =:= "publish" ->
	io:format("[~p] socket publish to Attribute=[~p].~n", [self(), Attribute]),
	{Attribute, publish, Message}.