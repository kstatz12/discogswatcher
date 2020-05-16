-module(schedule_server).

-behavior(gen_server).

-export([start_link/1, init/1]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

start_link(_) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    schedule_work(),
    {ok, []}.

handle_call(_, _From, State) ->
    {reply, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(send, State) ->
    schedule_work(),
    {noreply, State}.

schedule_work() ->
    io:format("Running Sending From ~p ~n", [self()]),
    Interval = timer:hours(6),
    timer:send_after(Interval, self(), send).








