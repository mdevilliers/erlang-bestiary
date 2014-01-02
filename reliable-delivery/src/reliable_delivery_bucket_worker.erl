-module (reliable_delivery_bucket_worker).

-behaviour(gen_server).

-export([start_link/1,empty_bucket/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Public API
start_link(Bucket) ->
  gen_server:start_link(?MODULE, [Bucket],[]).

empty_bucket(Pid, Bucket) ->
  gen_server:cast(Pid, {empty_bucket, Pid, Bucket}).

% others
init([Bucket]) ->
	%lager:info("~p started ~n", [Bucket]),
	reliable_delivery_event:subscribe(bucket_info),
	empty_bucket(self(), Bucket),
  {ok, [Bucket]}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({empty_bucket, Pid, Bucket}, State) ->
  do_empty_bucket(Pid, Bucket),
  {noreply, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({ _ ,{bucket_info},{new, _NewBucket}}, State) ->
  {stop, normal , State};
handle_info(_, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_, State, _) ->
  {ok, State}.

do_empty_bucket(Pid, Bucket) ->
 	case reliable_delivery_bucket_store:pop(Bucket) of
	{ok, {Identifier, _, Application, OffsetInBucket} } ->
		%lager:info("Popped ~p : ~p~n",[Bucket, Response]),
		reliable_delivery_monitor_sup:start_monitor(Identifier, OffsetInBucket, Application),
		do_empty_bucket(Pid, Bucket);
	{undefined} ->
		%lager:info("Empty ~p~n",[Bucket]),
		empty_bucket(Pid, Bucket)
end.