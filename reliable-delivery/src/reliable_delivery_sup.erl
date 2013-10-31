-module (reliable_delivery_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->

	UUidWorker = {reliable_delivery_uuid, 
					{reliable_delivery_uuid, start_link, []}, 
					permanent , 1000, worker, []},

	WorkerSupervisor = {reliable_delivery_monitor_sup, 
						{reliable_delivery_monitor_sup, start_link, []}, 
					 	 permanent, infinity, supervisor, []},

	MonitorStore = {reliable_delivery_monitor_store,
					{reliable_delivery_monitor_store, start_link, []},
					permanent,1000, worker,[]},

	StatsWorker = {reliable_delivery_monitor_stats,
					{reliable_delivery_monitor_stats,start_link,[]},
					permanent,1000,worker,[]},

    {ok, { {one_for_one, 5, 10}, [UUidWorker,WorkerSupervisor,MonitorStore,StatsWorker]} }.
