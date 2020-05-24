-module(api_worker_server).

-behavior(gen_server).

-export([start_link/1, init/1]).
-export([handle_call/3, handle_cast/2]).

-export([refresh/1]).

-record(state, {key, data}).

refresh(Pid) ->
    gen_server:cast(Pid, refresh).

start_link(Data) ->
    gen_server:start_link(?MODULE, Data, []).

init(Data) ->
    Key = generate_key(Data),
    State = #state{key = Key, data = Data},
    {ok, State}.

handle_call(_, _From, State) ->
    {reply, ok, State}.

handle_cast(refresh, State) ->
    {Artist, Title, Format, Token} = State#state.data,
    Masters = discogs:get_masters({Artist, Title, Format}, Token),
    Key = State#state.key,
    storage_server:put(Key, Masters),
    {noreply, State}.

generate_key(State) ->
    {Artist, Title, Format, _} = State,
    Concat = lists:flatten(io_lib:format("~s/~s/~s", [Artist, Title, Format])),
    BinStr = list_to_binary(Concat),
    <<Int:64, _/binary>> = murmur:murmur3_x64_128(BinStr),
    Int.


    

