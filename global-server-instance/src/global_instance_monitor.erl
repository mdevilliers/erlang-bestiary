-module (global_instance_monitor).
-behaviour (gen_server).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export ([start_monitor/0]).

% api
start_monitor() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% impl
start_monitoring() ->
	{ok,Pid} = global_instance_worker:start_global_worker(),
	case is_local_pid(Pid) of
		false ->
			monitor(process,Pid),
			loop(Pid);
		true  ->
			{ok,Pid}
	end.

loop(Pid) ->
	receive
		{'EXIT', _Pid, normal} ->
			io:format("Pid: ~p stopped. Reason:normal~n", [_Pid]),
			start_monitoring();
		{'DOWN', _MonitorReference, process, SomePid, Reason} ->
			io:format("Pid: ~p down. Reason: ~p~n", [SomePid, Reason]),
			io:format("Attempting re-start on this node~n"),
			start_monitoring();
		Msg ->
			io:format("Unknown Message: ~p~n", [Msg]),
			loop(Pid)
	end.

is_local_pid(Pid) ->
	node() =:= node(Pid).

% gen_server
init([]) ->
		io:format("monitor starting ~p~n", [self()]),
		start_monitoring(),
    	{ok, []}.

handle_call(Request, _From, State) ->
		io:format("monitor handle_call ~p~n", [Request]),
        Reply = ok,
        {reply, Reply, State}.

handle_cast(Msg, State) ->
		io:format("monitor handle_cast ~p~n", [Msg]),
        {noreply, State}.

handle_info(Info, State) ->
		io:format("monitor handle_info ~p ~p~n", [Info,State]),
    	{noreply, State}.

terminate(Reason, State) ->
		io:format("monitor terminate ~p ~p~n", [Reason, State]),
        ok.

code_change(_OldVsn, State, _Extra) ->
   		{ok, State}.
