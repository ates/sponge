-module(sponge).

%% API
-export([start/0, get/1, set/2, set/3, incr/1, incr/2]).

-include("sponge.hrl").

start() ->
    ok = application:start(sponge).


get(Key) ->
    gen_server:call(sponge_warehouse, {get, Key}).

set(Key, Value) ->
    set(Key, Value, ?DEFAULT_TTL).

set(Key, Value, TTL) when is_integer(TTL) ->
    Entry = #sponge_warehouse{key = Key, value = Value, ttl = TTL},
    gen_server:call(sponge_warehouse, {set, Entry}).

incr(Key) ->
    incr(Key, 1).

incr(Key, Incr) ->
    mnesia:dirty_update_counter(?TABLE, Key, Incr).
