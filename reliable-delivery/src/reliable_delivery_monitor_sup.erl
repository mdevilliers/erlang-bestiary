
-module (reliable_delivery_monitor_sup).

-behaviour(supervisor).

-export([start_link/0,start_monitor/4]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_monitor(Identifier, OffsetInBucket, Application, Value) ->
	supervisor:start_child(?MODULE,[Identifier,OffsetInBucket, Application, Value]).

init([]) ->
	Worker = { reliable_delivery_monitor,{reliable_delivery_monitor, start, []},
				temporary, brutal_kill, worker,[reliable_delivery_monitor]},
	Children = [Worker],
	
	RestartStrategy = {simple_one_for_one, 0 ,1},
    {ok, { RestartStrategy, Children}}.
