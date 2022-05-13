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
return_stash(Message, _, true) ->
	{Attribute, Map} = parse_message(Message, true),

	io:format("[~p] process Attribute=[~p].~n", [self(), Attribute]),
	gen_server:cast(sink_man, {Attribute, Map}),
	[].

parse_message(_BMessage, false) ->
	ErrorMessage = io_lib:format("[~p] PARSE ERROR! IS NOT JSON!~n", [self()]),
	error_logger:error_msg(ErrorMessage),
	exit(normal),
	{ok, ok};
parse_message(LMessage, true) ->
	BMessage = list_to_binary(LMessage),
	Map = jsx:decode(BMessage),
	#{<<"topic">> := Attribute} = Map,
	#{<<"param">> := Param} = Map,
	MParam = jsx:decode(Param),

	LAttribute = get_attribute(Attribute),

	{LAttribute, MParam}.

get_attribute(BAttribute) when is_binary(BAttribute) ->
	LAttribute = binary_to_list(BAttribute),
	LAttribute;
get_attribute(LAttribute) ->
	LAttribute.