-module (reliable_delivery_bucket_manager).

-behaviour(gen_server).

-export ([start_link/0, stop/0, current_bucket/0, get_bucket/1, get_bucket_duration/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include ("reliable_delivery.hrl").

%% Public API
current_bucket() ->
	gen_server:call(?MODULE, current_bucket).

get_bucket(PointInTime) ->
	gen_server:call(?MODULE, {get_bucket, PointInTime}).

get_bucket_duration() ->
  ?BUCKET_TICKS_PER_BUCKET * ?BUCKET_TICK_INTERVAL_MS.

start_link() ->
  gen_server:start({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:cast({local, ?MODULE}, stop).

init(_) ->
  timer:send_interval(?BUCKET_TICK_INTERVAL_MS, trigger),
  reliable_delivery_event:notify(bucket_info, {new, 0}),
	StartTime = time_util:now_in_seconds(),
  	{ok, #tick {
  		start_time = StartTime,
  		offset = 0,
  		bucket = 0
  	}}.

handle_call({get_bucket, PointInTime}, _,  #tick { bucket = BucketIdentifier, offset = Offset} = State) ->

  BucketDuration = get_bucket_duration(),
  OffsetInBucket = PointInTime rem BucketDuration,

  case trunc (PointInTime / BucketDuration) of
  	0  ->
  		Bucket = BucketIdentifier;
  	Any ->
  		Bucket = BucketIdentifier + Any
  end,

  {reply, { bucket, Bucket, OffsetInBucket + Offset}, State };
handle_call(current_bucket, _,  #tick { bucket = Bucket} = State) ->
  {reply, Bucket, State };
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(trigger, #tick { start_time = StartTime, offset = Offset, bucket = Bucket}) ->
	%lager:info("TicK ~p~n", [Tick]),
  Offset1 = Offset + 1,
	case Offset1 rem ?BUCKET_TICKS_PER_BUCKET  of
		0  ->
			Bucket1 = Bucket + 1,
      Offset2 = 0,
      reliable_delivery_event:notify(bucket_info, {new, Bucket1}),
      reliable_delivery_bucket_sup:start_bucket_worker(Bucket1) ;
		_ -> 
			Bucket1 = Bucket,
      Offset2 = Offset1
	end,
  
  NewState = #tick {
          start_time = StartTime,
          offset = Offset2,
          bucket = Bucket1},

	{noreply,NewState};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
  