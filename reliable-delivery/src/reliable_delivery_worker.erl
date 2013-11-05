-module (reliable_delivery_worker).

-behaviour(gen_server).
-export([start/3,notify_acked/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include ("reliable_delivery.hrl").

%% Public API

start(Identifier, LeaseTime, Value) ->
  gen_server:start_link(?MODULE, [Identifier, LeaseTime, Value],[]).

notify_acked(Pid) ->
  gen_server:call(Pid, acked).

init([Identifier,LeaseTime, Value]) ->

  reliable_delivery_monitor_store:insert(Identifier, self(), Value, LeaseTime),
  
  Now = calendar:local_time(),
  StartTime = calendar:datetime_to_gregorian_seconds(Now),
  
  reliable_delivery_monitor_stats:increment_total_monitors(),
  reliable_delivery_monitor_stats:increment_current_monitors(),
  
  {ok, 
    #lease{
      identifier = Identifier,
      lease_time = LeaseTime, 
      start_time = StartTime
  }, LeaseTime}.

handle_call(acked, _From, State) ->
  {stop, normal, ok, State};

% others
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(timeout, State) ->
  Identifier = State#lease.identifier,

  case reliable_delivery_monitor_store:lookup(Identifier) of
    {ok, _, Value, _, _} ->
      reliable_delivery_monitor_stats:increment_expired_monitors(),
      reliable_delivery:callback(expired, Identifier, Value);
    {error, not_found} ->
      reliable_delivery_monitor_stats:increment_unknown_monitors(),
      reliable_delivery:callback(unknown, Identifier, none)
  end,
  {stop, normal,State};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, State) ->
  Identifier = State#lease.identifier,
  reliable_delivery_monitor_store:delete(Identifier),
  reliable_delivery_monitor_stats:decrement_current_monitors(),
  ok.

code_change(_, State, _) ->
  {ok, State}.
