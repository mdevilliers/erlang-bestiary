-module (global_instance_worker).
-behaviour (gen_server).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export ([start_global_worker/0]).

% api
start_global_worker() ->
	case gen_server:start_link({global, ?MODULE}, ?MODULE, [], []) of
	    {ok, Pid} -> 
	        {ok, Pid};
	    {error, {already_started, Pid}} -> 
	    	io:format("Worker already started on ~p node~n", [Pid]), 
	        {ok, Pid};
	    Else -> Else
	end.

% gen_server
init([]) ->
	io:format("worker started on ~p~n", [self()]),
	erlang:send_after(500, self(), {tick}),
	{ok, []}.

handle_call(Request, _From, State) ->
	io:format("worker handle_call ~p.~n", [Request]),
    Reply = ok,
    {reply, Reply, State}.

handle_cast(Msg, State) ->
	io:format("worker handle_cast ~p.~n", [Msg]),
    {noreply, State}.

handle_info({tick}, State) ->
	io:format(".~n"),
	erlang:send_after(500, self(), {tick}),
	{noreply, State};
handle_info({stopworkerhard}, _State) ->
	throw(stopworkerhard);
handle_info({stopworker}, State) ->
	{stop,stopworker,State};
handle_info(Info, State) ->
	io:format("worker handle_info ~p ~p~n", [Info,State]),
	{noreply, State}.

terminate(Reason, State) ->
	io:format("worker terminate ~p ~p~n", [Reason, State]),
    ok.

code_change(_OldVsn, State, _Extra) ->
   	{ok, State}.