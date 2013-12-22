-module (reliable_delivery_bucket_sup).

-behaviour(supervisor).

-export([start_link/0,start_bucket_worker/1]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_bucket_worker(Bucket) ->
	supervisor:start_child(?MODULE,[Bucket]).

init([]) ->

	Worker = { reliable_delivery_bucket_worker,{reliable_delivery_bucket_worker, start_link, []},
				temporary, brutal_kill, worker,[reliable_delivery_bucket_worker]},
	Children = [Worker],
	
	RestartStrategy = {simple_one_for_one, 0 ,1},
    {ok, { RestartStrategy, Children}}.