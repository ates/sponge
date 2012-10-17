-module(sponge_warehouse).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-include("sponge.hrl").

-record(state, {ttl :: pos_integer()}).

start_link() ->
    Nodes = sponge_lib:get_option(nodes, []),
    TTL = sponge_lib:get_option(ttl, ?DEFAULT_TTL),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Nodes, TTL], []).

init([Nodes, TTL]) ->
    lists:foreach(fun(N) -> net_kernel:connect_node(N) end, Nodes),
    case nodes() of
        [] ->
            ?INFO("No nodes were found~n", []),
            mnesia:create_schema([node()]),
            mnesia:start(),
            mnesia:create_table(?TABLE, [
                {ram_copies, [node()]},
                {attributes, record_info(fields, ?TABLE)}
            ]);
        N ->
            ?INFO("Found active nodes: ~p~n", [N]),
            mnesia:start(),
            mnesia:change_config(extra_db_nodes, N),
            mnesia:add_table_copy(?TABLE, node(), ram_copies),
            mnesia:wait_for_tables([?TABLE], 30000)
    end,
    {ok, #state{ttl = TTL}}.

handle_call({get, Key}, _From, State) ->
    Now = sponge_lib:timestamp(),
    Result = case mnesia:dirty_read(?TABLE, Key) of
        [] ->
            undefined;
        [Entry] when Entry#sponge_warehouse.expired_at =< Now andalso Entry#sponge_warehouse.expired_at /= 0 ->
            sponge_sweeper:sweep(Key),
            undefined;
        [Entry] when is_record(Entry, ?TABLE) ->
            Entry#sponge_warehouse.value
    end,
    {reply, Result, State};
handle_call({set, Key, Value, TTL}, _From, State) when is_integer(TTL) ->
    do_set(Key, Value, TTL),
    {reply, ok, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast({incr, Key, Incr}, State) ->
    case mnesia:dirty_read(?TABLE, Key) of
        [Result] when is_integer(Result#sponge_warehouse.value) ->
            Entry = Result#sponge_warehouse{
                value = Result#sponge_warehouse.value + Incr
            },
            do_transaction(Entry);
        [Result] when is_record(Result, ?TABLE) ->
            ?ERROR("The value for key ~p can't be incremented~n", [Key]);
        [] ->
            do_set(Key, Incr, State#state.ttl)
    end,
    {noreply, State};

handle_cast({decr, Key, Decr}, State) ->
    case mnesia:dirty_read(?TABLE, Key) of
        [Result] when is_integer(Result#sponge_warehouse.value) ->
            Entry = Result#sponge_warehouse{
                value = Result#sponge_warehouse.value - Decr
            },
            do_transaction(Entry);
        [Result] when is_record(Result, ?TABLE) ->
            ?ERROR("The value for key ~p can't be decremented~n", [Key]);
        [] ->
            do_set(Key, Decr, State#state.ttl)
    end,
    {noreply, State};

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Reason, _State) -> ok.

%% Internal functions

-spec do_transaction(#sponge_warehouse{}) -> ok.
do_transaction(#sponge_warehouse{key = Key} = Entry) ->
    case mnesia:transaction(fun() -> mnesia:write(Entry) end) of
        {atomic, ok} -> ok;
        {aborted, Reason} ->
            ?ERROR("Can't perform transaction for key ~p due to: ~s~n",
                [Key, mnesia:error_description(Reason)])
    end.

do_set(Key, Value, TTL) ->
    ExpiredAt = case TTL of
        0 -> 0;
        _ ->
            sponge_lib:timestamp() + TTL
    end,
    Entry = #sponge_warehouse{
        key = Key, value = Value,
        expired_at = ExpiredAt
    },
    do_transaction(Entry).
