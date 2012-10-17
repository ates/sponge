-module(sponge).

%% API
-export([start/0, get/1, set/2, set/3, incr/1, incr/2]).
-export([uptime/0, debug/1]).

-include("sponge.hrl").

start() ->
    application:start(compiler),
    application:start(syntax_tools),
    application:start(lager),
    application:start(sponge).

get(Key) ->
    gen_server:call(sponge_warehouse, {get, Key}).

set(Key, Value) ->
    TTL = sponge_lib:get_option(ttl, ?DEFAULT_TTL),
    set(Key, Value, TTL).

set(Key, Value, TTL) when is_integer(TTL) ->
    gen_server:call(sponge_warehouse, {set, Key, Value, TTL}).

incr(Key) ->
    incr(Key, 1).

incr(Key, Incr) ->
    gen_server:cast(sponge_warehouse, {incr, Key, Incr}).

-spec uptime() -> {non_neg_integer(), calendar:time()}.
uptime() ->
    {T, _} = erlang:statistics(wall_clock),
    calendar:seconds_to_daystime(erlang:trunc(T / 1000)).

-spec debug(on | off) -> ok.
debug(on) -> lager:set_loglevel(lager_file_backend, "log/debug.log", debug);
debug(off) -> lager:set_loglevel(lager_file_backend, "log/debug.log", none).
