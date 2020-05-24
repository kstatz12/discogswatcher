-module(schedule_server).

-behavior(gen_server).

%% Server Exports
-export([start_link/1, init/1]).
-export([handle_call/3, handle_cast/2]).

%% API Exports
-export([start_scheduler/0]).

-record(state, {interval, record, worker_pid}).

start_scheduler() ->
    gen_server:cast(?MODULE, send).

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

init(Args) ->
    {Interval, Record} = Args,
    schedule_work(Interval),
    State = #state{interval = Interval, record = Record},
    {ok, State}.

handle_call(_, _From, State) ->
    {reply, ok, State}.

handle_cast(send, State) ->
    Duration = State#state.interval,
    schedule_work(Duration),
    {noreply, State};
handle_cast(_, State) ->
    {noreply, State}.


%% Internal Functions
schedule_work(Interval) when is_integer(Interval) ->
    io:format("Running Sending From ~p ~n", [self()]),
    Interval = timer:hours(Interval),
    timer:send_after(Interval, self(), send).








