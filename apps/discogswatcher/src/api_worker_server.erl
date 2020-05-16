-module(api_worker_server).

-behavior(gen_server).

-export([start_link/1, init/1]).
-export([handle_call/3, handle_cast/2]).

-record(state, {artist, title, format}).

start_link(Record) ->
    gen_server:start_link(?MODULE, Record, []).

init(Record) ->
    {Artist, Title, Format} = Record,
    {ok, #state{artist = Artist, title = Title, format = Format}}.

handle_call(_, _From, State) ->
    {reply, ok, State}.

handle_cast(request_update, State) ->
    {noreply, State}.
    

