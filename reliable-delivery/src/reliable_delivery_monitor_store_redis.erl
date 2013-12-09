-module (reliable_delivery_monitor_store_redis).

-behaviour(gen_server).

-export ([start_link/0, push_to_bucket/4, pop_from_bucket/1, ack_with_identifier/1, get_state/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include ("reliable_delivery.hrl").

%% Public API
start_link() ->
  gen_server:start({local, ?MODULE}, ?MODULE, [], []).

push_to_bucket(Identifier, LeaseTime,Application, Value) ->
	gen_server:call(?MODULE, {push, Identifier, LeaseTime,Application, Value }).

pop_from_bucket(Bucket) ->
	gen_server:call(?MODULE, {pop, Bucket}).

ack_with_identifier(Identifier) ->
	gen_server:call(?MODULE, {ack, Identifier}).

get_state(Identifier) ->
	gen_server:call(?MODULE, {get_state, Identifier}).

init([]) ->
  	{ok, ERedisPid} = eredis:start_link(),
  	{ok, ERedisPid}.


handle_call({get_state, Identifier}, _From, ERedisPid) ->

	case eredis:q(ERedisPid, ["GET", get_identifier_state_key (Identifier) ]) of
		{ok, <<"inprogress">>} ->
			Reply = {ok, inprogress};
		{ok, <<"acked">>} ->
			Reply = {ok, acked};
		{ok, <<"inmemory">>} ->
			Reply = {ok, inmemory};
		_ ->
			Reply = {ok, unknown}
	end,
	{reply, Reply ,ERedisPid};

handle_call({ack, Identifier}, _From, ERedisPid) ->

	% TODO - error handling
	% TODO - clean up other keys
	% TODO - check for already acked

	BucketKey = get_identifier_bucket_key (Identifier),
	{ok, Bucket} = eredis:q(ERedisPid, ["GET", BucketKey]),
	lager:info("Ack bucket - ~p~n", [Bucket]),
	lager:info("Ack bucket key ~p~n", [get_bucket_key (binary_to_list(Bucket))]),
	{ok, Members} = eredis:q(ERedisPid,["SMEMBERS", get_bucket_key (binary_to_list(Bucket)) ]),
    lager:info("Ack members - ~p~n", [Members]),
	Pipeline = [
				% remove from bucket
				["SREM", get_bucket_key (Bucket) , Identifier],
				% set state to acked
 				["SET", get_identifier_state_key (Identifier)  , <<"acked">>]
			   ],

	[{ok, _}, {ok, _}] = Reponse = eredis:qp(ERedisPid, Pipeline),
	lager:info("Ack response - ~p~n", [Reponse]),
	reliable_delivery_monitor_stats:decrement_persisted_monitors(),
	{reply, ok ,ERedisPid};

handle_call({pop, Bucket}, _From, ERedisPid) ->
	BucketName  = get_bucket_key(Bucket),
    lager:info("Pop bucket key ~p~n", [get_bucket_key(Bucket)]),
	case eredis:q(ERedisPid, ["SPOP", BucketName ]) of
		{ok, undefined} ->
			Reply = {undefined};
		{ok, Identifier } ->
			% not nice
			eredis:q(ERedisPid,["SET", get_identifier_state_key (Identifier)  , <<"inmemory">>]),
			{ok, Details} = eredis:q(ERedisPid,["GET", get_details_bucket_key (Identifier)]),
			Reply = {ok, binary_to_term(Details)}
	end,

  	{reply,Reply,ERedisPid};

handle_call({push, Identifier, LeaseTime, Application, Value}, _From, ERedisPid) ->

	{ bucket, Bucket, OffsetInBucket } = reliable_delivery_bucket_manager:get_bucket(LeaseTime),
	
	lager:info("push ~p into ~p with offset ~p~n", [Identifier,Bucket, OffsetInBucket]),
	lager:info("push bucket key ~p~n", [get_bucket_key(Bucket)]),
	Pipeline = [
					% push monitor to list
					["SADD", get_bucket_key(Bucket) , Identifier],
					% add details
			        ["SET", get_details_bucket_key (Identifier)  , {Identifier, LeaseTime, Application, OffsetInBucket}],
					% add to set of identifiers to ack
			        ["SET", get_identifier_bucket_key (Identifier)  , Bucket],
			        % save value against identifier
			 	    ["SET", get_identifier_value_key (Identifier)  , Value],
			 	    % set state
			        ["SET", get_identifier_state_key (Identifier)  , <<"inprogress">>]
        		],
	[{ok, _}, {ok, _}, {ok, _},{ok, _},{ok, _}] = Reponse = eredis:qp(ERedisPid, Pipeline),
	lager:info("push response - ~p~n", [Reponse]),
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
	io_lib:format("_rd:id:~p",[binary_to_list(Identifier)]).

get_details_bucket_key (Identifier) ->
	get_identifier_key (Identifier) ++ ":details".

get_identifier_value_key (Identifier) ->
	get_identifier_key (Identifier) ++ ":value".

get_identifier_bucket_key (Identifier) ->
	get_identifier_key (Identifier) ++ ":bucket".

get_identifier_state_key (Identifier) ->
	get_identifier_key (Identifier) ++ ":state".

get_bucket_key (Bucket) ->
	io_lib:format("_rd:bucket:~p",[Bucket]).

%get_ackable_bucket_key (Bucket) ->
%	get_bucket_key (Bucket) ++ ":ackable".