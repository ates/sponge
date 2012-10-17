-module(sponge_warehouse).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-include("sponge.hrl").

-record(state, {}).

start_link() ->
    Nodes = get_option(nodes, []),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Nodes], []).

init([Nodes]) ->
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
    {ok, #state{}}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Reason, _State) -> ok.

get_option(Option, Default) ->
    case application:get_env(sponge, Option) of
        {ok, Value} ->
            Value;
        undefined ->
            Default
    end.
