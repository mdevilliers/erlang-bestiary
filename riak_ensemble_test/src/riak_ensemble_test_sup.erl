-module (riak_ensemble_test_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	application:set_env(riak_ensemble, data_root, "temp"),
	
	EnsembleSupervisor = {riak_ensemble_sup, 
						{riak_ensemble_sup, start_link, []}, 
					 	 permanent, infinity, supervisor, []},


    {ok, { {one_for_one, 5, 10}, [EnsembleSupervisor]} }.
