-module (global_instance_worker).
-behaviour (gen_server).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export ([start_global_worker/0,start_monitoring/1]).

start_monitoring(Pid) ->
	process_flag(trap_exit, true),
	link(Pid),
	receive
		{'EXIT', Pid, Why} -> 
			io:format("Pid: ~p died. Reason: ~p~n", [Pid, Why]),
			io:format("Attempting re-start on this node~n"),
			{ok,Pid2} = start_global_worker(),
			start_monitoring(Pid2)
	end.

start_global_worker() ->
	case gen_server:start_link({global, ?MODULE}, ?MODULE, [], []) of
	    {ok, Pid} -> 
	        {ok, Pid};
	    {error, {already_started, Pid}} -> 
	    	io:format("Already started on ~p node~n", [Pid]), 
	        spawn(fun() -> start_monitoring(Pid) end),
	        {ok, Pid};
	    Else -> Else
	end.

init([]) ->
		process_flag(trap_exit, true),
		io:format("worker starting ~p~n", [self()]),
    	{ok, []}.

handle_call(_Request, _From, State) ->
		io:format("worker handle_call~n", []),
        Reply = ok,
        {reply, Reply, State}.

handle_cast(_Msg, State) ->
		io:format("worker handle_cast~n", []),
        {noreply, State}.

handle_info(Info, State) ->
		io:format("worker handle_info ~p ~p~n", [Info,State]),
    	{noreply, State}.

terminate(Reason, State) ->
		io:format("worker terminate ~p ~p~n", [Reason, State]),
        ok.

code_change(_OldVsn, State, _Extra) ->
   		{ok, State}.