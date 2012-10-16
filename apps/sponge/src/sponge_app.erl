-module(sponge_app).

-behaviour(application).

%% application callbacks
-export([start/2, stop/1]).

start(_Type, _Args) ->
    sponge_sup:start_link().

stop(_State) -> ok.

%% Internal functions

%initialize_mnesia() ->
%    case net_adm:world() of
%        [] ->
%            ?INFO("No additional nodes were found~n", []),
%            mnesia:create_schema([node()]),
%            mnesia:start(),
%            mnesia:create_table(entry, [{index, [key]}, {attributes, record_info(fields, entry)}]),
%            {atomic, ok} = mnesia:add_table_copy(entry, node(), ram_copies);
%
%        Nodes when is_list(Nodes) ->
%            ?INFO("Found clustered nodes: ~p~n", [Nodes]),
%            mnesia:start(),
%            mnesia:change_config(extra_db_nodes, Nodes),
%            {atomic, ok} = mnesia:add_table_copy(entry, node(), ram_copies),
%            ok = mnesia:wait_for_tables([entry], 30000)
%    end,
%    ok.
