-module(sponge_lib).

-export([get_option/1, get_option/2]).

get_option(Option) ->
    get_option(Option, undefined).

get_option(Option, Default) ->
    case application:get_env(sponge, Option) of
        {ok, Value} ->
            Value;
        undefined ->
            Default
    end.
