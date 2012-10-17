-module(sponge_lib).

-export([get_option/1, get_option/2, timestamp/0]).

get_option(Option) ->
    get_option(Option, undefined).

get_option(Option, Default) ->
    case application:get_env(sponge, Option) of
        {ok, Value} ->
            Value;
        undefined ->
            Default
    end.

timestamp() ->
    {MegaSecs, Secs, _} = os:timestamp(),
    MegaSecs * 1000000 + Secs.
