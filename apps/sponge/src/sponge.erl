-module(sponge).

%% API
-export([start/0, uptime/0, debug/1]).

-include("sponge.hrl").

start() ->
    application:start(compiler),
    application:start(syntax_tools),
    application:start(lager),
    application:start(sponge).

-spec uptime() -> {non_neg_integer(), calendar:time()}.
uptime() ->
    {T, _} = erlang:statistics(wall_clock),
    calendar:seconds_to_daystime(erlang:trunc(T / 1000)).

-spec debug(on | off) -> ok.
debug(on) -> lager:set_loglevel(lager_file_backend, "log/debug.log", debug);
debug(off) -> lager:set_loglevel(lager_file_backend, "log/debug.log", none).
