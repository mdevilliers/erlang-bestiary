-module (global_instance_monitor).
-behaviour (gen_server).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export ([start_monitoring/0]).

start_monitoring() ->
	{ok,Pid} = global_instance_worker:start_global_worker(),
	start_monitoring(Pid).
start_monitoring(Pid) ->
	process_flag(trap_exit, true),
	link(Pid),
	receive
		{'EXIT', Pid, Why} -> 
			io:format("Pid: ~p died. Reason: ~p~n", [Pid, Why]),
			io:format("Attempting re-start on this node~n"),
			{ok,Pid2} = global_instance_worker:start_global_worker(),
			start_monitoring(Pid2)
	end.

init([]) ->
		io:format("monitor starting ~p~n", [self()]),
    	{ok, []}.

handle_call(_Request, _From, State) ->
		io:format("monitor handle_call~n", []),
        Reply = ok,
        {reply, Reply, State}.

handle_cast(_Msg, State) ->
		io:format("monitor handle_cast~n", []),
        {noreply, State}.

handle_info(Info, State) ->
		io:format("monitor handle_info ~p ~p~n", [Info,State]),
    	{noreply, State}.

terminate(Reason, State) ->
		io:format("monitor terminate ~p ~p~n", [Reason, State]),
        ok.

code_change(_OldVsn, State, _Extra) ->
   		{ok, State}.
