-module(sponge_sweeper).

-behaviour(gen_server).

%% API
-export([start_link/0, schedule_sweep/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-include("sponge.hrl").

-record(state, {entries = []}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

schedule_sweep(Key, TTL) ->
    gen_server:cast(?MODULE, {sweep, Key, TTL}).

init([]) ->
    {ok, #state{}}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast({sweep, Key, TTL}, State) ->
    case lists:member(Key, State#state.entries) of
        true ->
            ?DEBUG("The ~p entry is already scheduled to sweep~n", [Key]),
            {noreply, State};
        false ->
            {ok, _} = timer:send_after(TTL, ?MODULE, {sweep, Key}),
            {noreply, State#state{entries = [Key | State#state.entries]}}
    end;
handle_cast(_Msg, State) -> {noreply, State}.

handle_info({sweep, Key}, State) ->
    ?DEBUG("Sweeping the entry with key ~p~n", [Key]),
    ok = mnesia:dirty_delete(?TABLE, Key),
    {noreply, State#state{entries = State#state.entries -- [Key]}};

handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Reason, _State) -> ok.
