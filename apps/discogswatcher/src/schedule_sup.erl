-module(schedule_sup).

-behavior(supervisor).

-export([init/1]).
-export([start_link/0]).

init(_) ->
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    Children = [
                #{
                  id => schedule_server,
                  start => {schedule_server, start_link, []},
                  shutdown => brutal_kill,
                  type => worker,
                  modules => [schedule_server]
                  }
               ],
    {ok, {SupFlags, Children}}.

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).
