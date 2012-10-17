-module(sponge_sweeper).

-behaviour(gen_server).

%% API
-export([start_link/0, sweep/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-include("sponge.hrl").

-record(state, {timer :: reference(), ttl :: pos_integer()}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

sweep(Key) ->
    gen_server:cast(?MODULE, {sweep, Key}).

init([]) ->
    Timeout = sponge_lib:get_option(ttl, ?DEFAULT_TTL),
    Timer = erlang:send_after(Timeout, self(), expire_all),
    {ok, #state{timer = Timer, ttl = Timeout}}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast({sweep, Key}, State) ->
    sweep_entry(Key),
    {noreply, State};

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(expire_all, State) ->
    erlang:cancel_timer(State#state.timer),
    Now = sponge_lib:timestamp(),
    Guard = fun(E) -> E#sponge_warehouse.expired_at =< Now end,
    Fun = fun(E) -> sweep_entry(E#sponge_warehouse.key) end,
    traverse_all(Guard, Fun),
    Timer = erlang:send_after(State#state.ttl, self(), expire_all),
    {noreply, State#state{timer = Timer}};

handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Reason, _State) -> ok.

sweep_entry(Key) ->
    ?DEBUG("Sweeping the entry with key ~p~n", [Key]),
    mnesia:dirty_delete(?TABLE, Key).

traverse_all(Guard, Fun) ->
    Key = mnesia:dirty_first(?TABLE),
    traverse_all(Key, Guard, Fun).

traverse_all('$end_of_table', _, _) -> ok;
traverse_all(Key, Guard, Fun) ->
    case mnesia:dirty_read(?TABLE, Key) of
        [State] ->
            Next = mnesia:dirty_next(?TABLE, Key),
            case Guard(State) of
                true ->
                    Fun(State),
                    traverse_all(Next, Guard, Fun);
                _ ->
                    traverse_all(Next, Guard, Fun)
            end;
        [] ->
            traverse_all(Guard, Fun)
    end.
