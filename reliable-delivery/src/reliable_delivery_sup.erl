-module (reliable_delivery_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->

	BucketSupervisor = {reliable_delivery_bucket_sup, 
						{reliable_delivery_bucket_sup, start_link, []}, 
					 	 permanent, infinity, supervisor, []},

	BucketManager =  {reliable_delivery_bucket_manager, 
					{reliable_delivery_bucket_manager, start_link, []}, 
					permanent , 1000, worker, []},

	BucketKicker = {reliable_delivery_bucket_kicker,
					{reliable_delivery_bucket_kicker, start_link, []},
					permanent,1000, worker,[]},

	WorkerSupervisor = {reliable_delivery_monitor_sup, 
						{reliable_delivery_monitor_sup, start_link, []}, 
					 	 permanent, infinity, supervisor, []},

	RedisStore = {reliable_delivery_monitor_store_redis,
					{reliable_delivery_monitor_store_redis, start_link, []},
					permanent,1000, worker,[]},
					 	 
	MonitorStore = {reliable_delivery_monitor_store,
					{reliable_delivery_monitor_store, start_link, []},
					permanent,1000, worker,[]},

	StatsWorker = {reliable_delivery_monitor_stats,
					{reliable_delivery_monitor_stats,start_link,[]},
					permanent,1000,worker,[]},

    {ok, { {one_for_one, 5, 10}, [BucketSupervisor, BucketManager, BucketKicker, WorkerSupervisor, RedisStore, MonitorStore, StatsWorker]} }.
