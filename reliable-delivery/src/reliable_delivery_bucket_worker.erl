-module (reliable_delivery_bucket_worker).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Public API
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [],[]).

% others
init([]) ->
	reliable_delivery_event:subscribe(bucket_info),
    {ok, []}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.


handle_info({ _ ,{bucket_info},{new, NewBucket}}, State) ->
	empty_bucket(NewBucket),
	{noreply, State};
handle_info(_Info, State) ->
  lager:info("Info : ~p~n",[_Info]),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_, State, _) ->
  {ok, State}.

empty_bucket(Bucket) -> 
	case reliable_delivery_monitor_store_redis:pop_from_bucket(Bucket) of
		{ok, {Identifier, _, Application, OffsetInBucket} = Response} ->
			lager:info("Popped ~p : ~p~n",[Bucket, Response]),
			reliable_delivery_monitor_sup:start_monitor(Identifier, OffsetInBucket, Application),
			empty_bucket(Bucket);
		{undefined} ->
			lager:info("Empty ~p~n",[Bucket])
	end.