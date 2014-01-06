-module (reliable_delivery_monitor).

-behaviour(gen_server).
-export([start/4,notify_acked/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include ("reliable_delivery.hrl").

start(Identifier, OffsetInBucket, Application, Value) ->
  gen_server:start_link(?MODULE, [Identifier, OffsetInBucket, Application, Value],[]).

notify_acked(Pid) ->
  gen_server:call(Pid, acked).

init([Identifier, OffsetInBucket, Application, Value]) ->
  lager:info("Worker Started Identifier : ~p , OffsetInBucket ~p~n", [Identifier,OffsetInBucket]),
  reliable_delivery_monitor_store:insert(Identifier, self()),

  StartTime = time_util:now_in_seconds(),
    
  {ok, 
    #lease{
      identifier = Identifier,
      offset_in_bucket = OffsetInBucket, 
      start_time = StartTime,
      application = Application,
      value = Value
  }, OffsetInBucket}.

handle_call(acked, _From, State) ->
  %Identifier = State#lease.identifier, 
  %lager:info("acked ~p~n",[Identifier]), 
  {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(timeout, State) ->
  Identifier = State#lease.identifier,
  Application = State#lease.application,
  Value = State#lease.value,
  {ok, _ , _} = reliable_delivery_monitor_store:lookup(Identifier),
  reliable_delivery_bucket_store:expire_monitor(Identifier),
  try_to_send_expiration_to_connected_application(Identifier, Application, Value),
 
  {stop, normal,State};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, State) ->

  Identifier = State#lease.identifier,
  %lager:info("terminate : ~p : ~p~n", [Identifier, _Reason]),
  %reliable_delivery_bucket_store:expire_monitor(Identifier),
  reliable_delivery_monitor_store:delete(Identifier),
  %reliable_delivery_monitor_stats:decrement_current_monitors(),
  ok.

code_change(_, State, _) ->
  {ok, State}.

try_to_send_expiration_to_connected_application(Identifier,Application, Value) ->
  case  gproc:lookup_values({p, l, {application}}) of
    [{Pid,Application}|_]  ->
      reliable_delivery_monitor_stats:increment_expired_acknowledgement_delivery_succeded(),
      gproc:send(Pid, {expired, Identifier, Value});
    [] -> 
      reliable_delivery_monitor_stats:increment_expired_acknowledgement_delivery_failed(),
      lager:error("Expired item confirmation not sent ~p~n", [Identifier])
  end.
