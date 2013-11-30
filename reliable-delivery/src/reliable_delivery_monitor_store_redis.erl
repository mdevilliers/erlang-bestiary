-module (reliable_delivery_monitor_store_redis).

-behaviour(gen_server).

-export ([start_link/0, push_to_bucket/4, pop_from_bucket/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% Public API
start_link() ->
  gen_server:start({local, ?MODULE}, ?MODULE, [], []).

push_to_bucket(Identifier, LeaseTime,Application, Value) ->
	gen_server:call(?MODULE, {push, Identifier, LeaseTime,Application, Value }).

pop_from_bucket(Bucket) ->
	gen_server:call(?MODULE, {pop, Bucket}).

init([]) ->
  	{ok, ERedisPid} = eredis:start_link(),
  	{ok, ERedisPid}.

handle_call({pop, Bucket}, _From, ERedisPid) ->
	BucketName  = get_bucket_name (Bucket),
	Reply = eredis:q(ERedisPid, ["LPOP", BucketName ]),
  	{reply,Reply,ERedisPid};

handle_call({push, Identifier, LeaseTime, Application, Value}, _From, ERedisPid) ->

	{ bucket, Bucket, OffsetInBucket } = reliable_delivery_ticker:get_bucket(LeaseTime),
	BucketName  = get_bucket_name (Bucket),
	Reply = eredis:q(ERedisPid, ["LPUSH", BucketName , {Identifier, LeaseTime, Application, Value, OffsetInBucket}]),
  	{reply,Reply,ERedisPid};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

get_bucket_name (Bucket) ->
	io_lib:format("rd:bucket:~p",[Bucket]).