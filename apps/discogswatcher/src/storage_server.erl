-module(storage_server).
-behavior(gen_server).

-export([init/1, start_link/0]).
-export([handle_cast/2, handle_call/3, handle_info/2]).
-export([terminate/2, code_change/3]).
-export([put/2, get/1, delete/1]).

-record(state, {init = true, table_id}).

put(Key, Data) ->
    gen_server:cast(?MODULE, {put, Key, Data}).

get(Key) ->
    gen_server:call(?MODULE, {get, Key}).

delete(Key) ->
    gen_server:cast(?MODULE, {delete, Key}).

init(_) ->
    {ok, #state{}}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

handle_cast({put, Key, Data}, State) ->
    TableId = State#state.table_id,
    ets:insert(TableId, {Key, Data}),
    {noreply, State};
handle_cast({delete, Key}, State) ->
    TableId = State#state.table_id,
    ets:delete(TableId, Key),
    {noreply, State};
handle_cast(_, State) ->
    {noreply, State}.

handle_call({get, Key}, _From, State) ->
    TableId = State#state.table_id,
    Result = ets:lookup(TableId, Key),
    {reply, Result, State};
handle_call(_, _From, State) ->
    {reply, ok, State}.

    


handle_info({'ETC-TRANSFER', TableId, _Pid, _Data}, State) ->
    {noreply, State#state{table_id = TableId}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



