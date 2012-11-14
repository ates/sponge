-module(sponge_api).

%% API
-export([get/1, set/3, incr/2, decr/2]).

-include("sponge.hrl").

get(Key) ->
    gen_server:call(getpid(), {get, Key}).

set(Key, Value, TTL) ->
    gen_server:call(getpid(), {set, Key, Value, TTL}).

incr(Key, Incr) ->
    gen_server:cast(getpid(), {incr, Key, Incr}).

decr(Key, Decr) ->
    gen_server:cast(getpid(), {decr, Key, Decr}).

%% Internal functions
-spec getpid() -> pid() | {error, empty_process_group}.
getpid() ->
    Members = lists:map(fun(Pid) ->
        [{message_queue_len, Messages}] = erlang:process_info(Pid, [message_queue_len]),
        {Pid, Messages}
    end, pg2:get_members(?GROUP)),
    case lists:keysort(2, Members) of
        [{Pid, _} | _] -> Pid;
        [] -> {error, empty_process_group}
    end.
