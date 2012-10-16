-module(sponge).

%% API
-export([start/0, get/1, set/2, set/3, incr/1, incr/2]).

-include("sponge.hrl").

start() ->
    ok = application:start(sponge).


get(Key) ->
    mnesia:dirty_read(entry, Key).

set(Key, Value) ->
    set(Key, Value, ?DEFAULT_TTL).

set(Key, Value, TTL) ->
    Entry = #sponge_warehouse{key = Key, value = Value, ttl = TTL},
    F = fun() -> mnesia:write(?TABLE, Entry) end,
    case mnesia:transaction(F) of
        {atomic, ok} ->
            {ok, _} = timer:send_after(TTL, sponge_killer, {kill, Key});
        Error -> ?ERROR("SET ERROR: ~p~n", [Error])
    end.

incr(Key) ->
    incr(Key, 1).

incr(Key, Incr) ->
    mnesia:dirty_update_counter(?TABLE, Key, Incr).
