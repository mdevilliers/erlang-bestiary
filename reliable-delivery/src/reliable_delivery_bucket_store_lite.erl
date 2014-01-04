-module (reliable_delivery_bucket_store_lite).

-behaviour(gen_server).

-export ([start_link/0, stop/0, push_to_bucket/6, pop_from_bucket/1, ack_with_identifier/1, get_state_for_monitor/1, get_state/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include ("reliable_delivery.hrl").

-define (MONITOR_STATE_TABLE_ID, monitor_state_store).
-define (BUCKET_MONITOR_TABLE_ID, bucket_monitor_store).
-define (MONITOR_TABLE_ID, monitor_store).

%% Public API
start_link() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

push_to_bucket( Bucket, OffsetInBucket, Identifier, LeaseTime,Application, Value) ->
	gen_server:call(?MODULE, {push,  Bucket, OffsetInBucket, Identifier, LeaseTime, Application, Value }).

pop_from_bucket(Bucket) ->
	gen_server:call(?MODULE, {pop, Bucket}).

ack_with_identifier(Identifier) ->
	gen_server:call(?MODULE, {ack, Identifier}).

get_state_for_monitor(Identifier) ->
	gen_server:call(?MODULE, {get_state_for_monitor, Identifier}).

get_state(Bucket) ->
	gen_server:call(?MODULE, {get_state, Bucket}).

stop() ->
    gen_server:cast(?MODULE, {stop}).

init([]) ->
	ets:new(?MONITOR_STATE_TABLE_ID,[protected, named_table, {keypos, #bucket_monitor_state.identifier}]),
	ets:new(?BUCKET_MONITOR_TABLE_ID,[protected, named_table, bag, {keypos, #bucket_monitor.bucket}]),
	ets:new(?MONITOR_TABLE_ID,[protected, named_table, {keypos, #monitor.identifier}]),
  	{ok, []}.

handle_call({get_state, Bucket}, _From, State) ->
	
	case ets:lookup(?BUCKET_MONITOR_TABLE_ID, Bucket) of
		[] -> 
			Reply = {error, not_found};
		Records ->
			Reply = {ok, Records}
	end,
	{reply, Reply ,State};

handle_call({get_state_for_monitor, Identifier}, _From, State) ->
	
	case ets:lookup(?MONITOR_STATE_TABLE_ID, Identifier) of
		[{bucket_monitor_state, Identifier, _, MonitorState}] ->
			Reply = {ok, MonitorState};
		[] -> 
			Reply = {error, not_found}
	end,
	{reply, Reply ,State};

handle_call({ack, Identifier}, _From, State) ->
	
	% set new state
	true = ets:update_element(?MONITOR_STATE_TABLE_ID, Identifier, { 4, <<"acked">> }),

	% remove from bucket - i think this uses a table scan?
 	1 = ets:select_delete(?BUCKET_MONITOR_TABLE_ID , [{#bucket_monitor{ bucket = '_', identifier = '$1'}, [{'==','$1',{const,Identifier}}],[true]}]),

	% delete the monitor
	true = ets:delete(?MONITOR_TABLE_ID,Identifier),

	%reliable_delivery_monitor_stats:decrement_persisted_monitors(),
	{reply, ok ,State};

handle_call({pop, Bucket}, _, State) ->
 
	% get all records from bucket_monitor tablr
			% using above return from monitor table...
			% update record state

	case ets:lookup(?BUCKET_MONITOR_TABLE_ID, Bucket) of
		[] -> 
			Reply = {error, not_found};
		Records ->
			update_multiple_monitor_state(Records, <<"inmemory">>),
			Accumulated = accumulate_monitors(Records, []),
			Reply = {ok, Accumulated}
	end,

  	{reply,Reply,State};

handle_call({push, Bucket, OffsetInBucket, Identifier, LeaseTime, Application, Value}, _, State) ->

	true = ets:insert(?MONITOR_STATE_TABLE_ID, #bucket_monitor_state{identifier = Identifier, bucket = Bucket, state = <<"inprogress">>  }),
	true = ets:insert(?BUCKET_MONITOR_TABLE_ID, #bucket_monitor{bucket = Bucket, identifier = Identifier }),
	true = ets:insert(?MONITOR_TABLE_ID, #monitor{identifier = Identifier, offsetInBucket = OffsetInBucket, leaseTime = LeaseTime, application = Application, value = Value }),

	%reliable_delivery_monitor_stats:increment_persisted_monitors(),
  	{reply, ok ,State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({stop}, State) ->
  {stop, normal, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

update_multiple_monitor_state([], _) ->
	ok;
update_multiple_monitor_state([ #bucket_monitor{ identifier = Identifier } | T], State) ->
	true = ets:update_element(?MONITOR_STATE_TABLE_ID, Identifier, { 3, State }),
	update_multiple_monitor_state(T, State).

accumulate_monitors([] , Acc) ->
	Acc;
accumulate_monitors([ #bucket_monitor{ identifier = Identifier } | T] , Acc) ->
	case ets:lookup(?MONITOR_TABLE_ID,Identifier) of
		[{monitor, Identifier,  OffsetInBucket, LeaseTime, Application, Value }] ->
			accumulate_monitors(T, [{Identifier, LeaseTime, Application, OffsetInBucket, Value } | Acc]);
		[] -> 
			{error, identifier_not_found}
	end.

