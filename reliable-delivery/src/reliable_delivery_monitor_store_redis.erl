-module (reliable_delivery_monitor_store_redis).

-behaviour(gen_server).

-export ([start_link/0, push_to_bucket/4, pop_from_bucket/1, ack_with_identifier/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Public API
start_link() ->
  gen_server:start({local, ?MODULE}, ?MODULE, [], []).

push_to_bucket(Identifier, LeaseTime,Application, Value) ->
	gen_server:call(?MODULE, {push, Identifier, LeaseTime,Application, Value }).

pop_from_bucket(Bucket) ->
	gen_server:call(?MODULE, {pop, Bucket}).

ack_with_identifier(Identifier) ->
	gen_server:call(?MODULE, {ack, Identifier}).

init([]) ->
  	{ok, ERedisPid} = eredis:start_link(),
  	{ok, ERedisPid}.

handle_call({ack, Identifier}, _From, ERedisPid) ->

	% TODO - error handling
	% TODO - clean up other keys
	
	{ok, Bucket} = get_identifier_bucket_key (Identifier),
	{ok, _} = eredis:q(ERedisPid, ["SREM", get_ackable_bucket_key (Bucket) , Identifier]),

	reliable_delivery_monitor_stats:decrement_persisted_monitors(),
	{reply, ok ,ERedisPid};

handle_call({pop, Bucket}, _From, ERedisPid) ->
	BucketName  = get_bucket_key(Bucket),

	case eredis:q(ERedisPid, ["LPOP", BucketName ]) of
		{ok, undefined} ->
			Reply = {undefined};
		{ok, Bin } ->
			Reply = {ok, binary_to_term(Bin)}
	end,

  	{reply,Reply,ERedisPid};

handle_call({push, Identifier, LeaseTime, Application, Value}, _From, ERedisPid) ->

	{ bucket, Bucket, OffsetInBucket } = reliable_delivery_bucket_manager:get_bucket(LeaseTime),
	
	%TODO - find out half of offset push to left < half , push to right > half
	lager:info("push ~p", [Identifier]),
	Pipeline = [
					% push monitor to list
					["LPUSH", get_bucket_key(Bucket) , {Identifier, LeaseTime, Application, OffsetInBucket}],
					% add to set of identifiers to ack
			        ["SADD", get_ackable_bucket_key (Bucket) , Identifier],
			        % save bucket against identifer 
			        ["SET", get_identifier_bucket_key (Identifier)  , Bucket],
			        % save value against identifier
			 	    ["SET", get_identifier_value_key (Identifier)  , Value]
        		],
	[{ok, _}, {ok, _}, {ok, _}, {ok, _}] = eredis:qp(ERedisPid, Pipeline),
	reliable_delivery_monitor_stats:increment_persisted_monitors(),
  	{reply, ok ,ERedisPid};

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

get_identifier_key (Identifier) ->
	io_lib:format("rd:id:~p",[binary_to_list(Identifier)]).

get_identifier_value_key (Identifier) ->
	get_identifier_key (Identifier) ++ ":value".

get_identifier_bucket_key (Identifier) ->
	get_identifier_key (Identifier) ++ ":bucket".

get_bucket_key (Bucket) ->
	io_lib:format("rd:bucket:~p",[Bucket]).

get_ackable_bucket_key (Bucket) ->
	get_bucket_key (Bucket) ++ ":ackable".