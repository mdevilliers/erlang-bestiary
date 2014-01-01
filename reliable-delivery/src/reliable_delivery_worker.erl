-module (reliable_delivery_worker).

-behaviour(gen_server).
-export([start/3,notify_acked/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include ("reliable_delivery.hrl").

start(Identifier,LeaseTime,Application) ->
  gen_server:start_link(?MODULE, [Identifier, LeaseTime, Application],[]).

notify_acked(Pid) ->
  gen_server:call(Pid, acked).

init([Identifier,LeaseTime,Application]) ->
  lager:info("Worker Started Identifier : ~p , LeaseTime ~p~n", [Identifier,LeaseTime]),
  reliable_delivery_monitor_store:insert(Identifier, self()),

  StartTime = time_util:now_in_seconds(),
    
  {ok, 
    #lease{
      identifier = Identifier,
      lease_time = LeaseTime, 
      start_time = StartTime,
      application = Application
  }, LeaseTime}.

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
  %Application = State#lease.application,
  case reliable_delivery_monitor_store:lookup(Identifier) of
    {ok, _ , _} ->
      %lager:info("timeout ~p~n",[Identifier]),
      reliable_delivery_monitor_stats:increment_expired_monitors();
      %try_to_send_expiration_to_connected_application(Identifier, Application, Value);
    {error, not_found} ->
      lager:info("xxxxx this should never happen xxxxxx ~p~n",[Identifier]),
      reliable_delivery_monitor_stats:increment_unknown_monitors(),
      reliable_delivery:callback(unknown, Identifier, none)
  end,
  {stop, normal,State};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, State) ->

  Identifier = State#lease.identifier,
  %lager:info("terminate : ~p : ~p~n", [Identifier, _Reason]),
  reliable_delivery_monitor_store:delete(Identifier),
  reliable_delivery_monitor_stats:decrement_current_monitors(),
  ok.

code_change(_, State, _) ->
  {ok, State}.

%try_to_send_expiration_to_connected_application(Identifier,Application, Value) ->
%      case  gproc:lookup_values({p, l, {application}}) of
%        [{Pid,Application}|_]  ->
%          reliable_delivery_monitor_stats:increment_expired_acknowledgement_delivery_succeded(),
%          gproc:send(Pid, {expired, Identifier, Value});
%        [] -> 
%          reliable_delivery_monitor_stats:increment_expired_acknowledgement_delivery_failed(),
%          lager:error("Expired item confirmation not sent ~p~n", [Identifier])
%      end.
