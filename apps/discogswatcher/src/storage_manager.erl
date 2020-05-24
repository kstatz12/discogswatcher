-module(storage_manager).

-behavior(gen_server).

-export([init/1, start_link/0]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {table_id}).

give_away() ->
    gen_server:cast(?MODULE, {give_away, []}).

init(_) ->
    process_flag(trap_exit, true),
    give_away(),
    {ok, #state{}}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

handle_call(_, _From, State) ->
    {reply, ok, State}.

handle_cast({give_away, Data}, State) ->
    Server = wait_for_server(),
    link(Server),
    TableId = ets:new(discogs_storage, [set]),
    ets:setopts(TableId, {heir, self(), Data}),
    ets:give_away(TableId, Server, Data),
    {noreply, State#state{table_id = TableId}};

handle_cast(_, State) ->
    {noreply, State}.

handle_info({'EXIT', _Pid, killed}, State) ->
    {noreply, State};
handle_info({'ETS-TRANSFER', TableId, _Pid, Data}, State) ->
    Server = wait_for_server(),
    link(Server),
    ets:give_away(TableId, Server, Data),
    {noreply, State#state{table_id = TableId}};
handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

wait_for_server() ->
    case whereis(storage_server) of
        undefined ->
            timer:sleep(1),
            wait_for_server();
        Pid -> Pid
     end.
