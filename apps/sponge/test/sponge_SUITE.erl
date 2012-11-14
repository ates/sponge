-module(sponge_SUITE).

-include_lib("common_test/include/ct.hrl").

%% CT callbacks
-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% tests
-export([set_get/1, incr_decr/1]).

all() -> [set_get, incr_decr].

init_per_suite(Config) ->
    sponge:start(),
    Config.

end_per_suite(Config) ->
    application:stop(sponge),
    Config.

init_per_testcase(_TestCase, Config) ->
    ok = sponge_sweeper:sweep(foo),
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.

set_get(_Config) ->
    ok = sponge_api:set(foo, bar, 0),
    bar = sponge_api:get(foo).

incr_decr(_Config) ->
    ok = sponge_api:incr(foo, 10),
    10 = sponge_api:get(foo),
    ok = sponge_api:decr(foo, 5),
    5 = sponge_api:get(foo).
