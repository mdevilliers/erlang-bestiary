-module(global_instance_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).	

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
	io:format("supervisor starting~n", []),
 	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->

	GlobalWorker = 
				{global_worker, 
					{global_instance_worker, start_global_worker, []}, 
					permanent , 1000, worker, []},

    {ok, { {one_for_one, 5, 10}, [GlobalWorker]} }.