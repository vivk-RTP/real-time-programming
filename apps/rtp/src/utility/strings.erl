%%%-------------------------------------------------------------------
%%% @author Volcov Oleg
%%% @copyright (C) 2022, FAF-191
%%% @doc Module for all additional functions associated with strings.
%%% @end
%%%-------------------------------------------------------------------

-module(strings).

-export([replace/3]).

%%--------------------------------------------------------------------
%% @doc Replace all @ReplaceOf substrings in @Source string with
%%      @ReplaceOn substring.
%% @end
%%--------------------------------------------------------------------

replace(Source, ReplaceOf, ReplaceOn) ->
	re:replace(Source, ReplaceOf, ReplaceOn, [global, {return, list}]).
