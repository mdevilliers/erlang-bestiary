-module (reliable_delivery_bucket_store_lite).

-behaviour(gen_server).

-export ([start_link/0, stop/0, push_to_bucket/6, pop_from_bucket/1, ack_with_identifier/1, get_state_for_monitor/1, get_state/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include ("reliable_delivery.hrl").

%% Public API
start_link() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

push_to_bucket( Bucket, OffsetInBucket, Identifier, LeaseTime,Application, Value) ->
	gen_server:call(?MODULE, {push,  Bucket, OffsetInBucket, Identifier, LeaseTime,Application, Value }).

pop_from_bucket(Bucket) ->
	gen_server:call(?MODULE, {pop, Bucket}).

ack_with_identifier(Identifier) ->
	gen_server:call(?MODULE, {ack, Identifier}).

get_state_for_monitor(Identifier) ->
	gen_server:call(?MODULE, {get_state_for_monitor, Identifier}).

get_state() ->
	gen_server:call(?MODULE, {get_state}).

stop() ->
    gen_server:cast(?MODULE, {stop}).

init([]) ->
  	{ok, []}.


 %State [[2,
 %        {monitors,[[{<<"inprogress">>,100,<<"ghi">>,1000,
 %                     <<"my application">>,<<"my value">>}]]}],
 %       [1,
 %        {monitors,[[{<<"inprogress">>,100,<<"def">>,1000,
 %                     <<"my application">>,<<"my value">>}],
 %                   [{<<"inprogress">>,100,<<"abc">>,1000,
 %                     <<"my application">>,<<"my value">>}]]}]]

handle_call({get_state}, _From, State) ->
	{reply, State ,State};
handle_call({get_state_for_monitor, _Identifier}, _From, State) ->

	%case eredis:q(ERedisPid, ["GET", get_identifier_state_key (Identifier) ]) of
	%	{ok, <<"inprogress">>} ->
			Reply = {ok, inprogress},
	%	{ok, <<"acked">>} ->
	%		Reply = {ok, acked};
	%	{ok, <<"inmemory">>} ->
	%		Reply = {ok, inmemory};
	%	_ ->
	%		Reply = {ok, unknown}
	%end,

	{reply, Reply ,State};

handle_call({ack, Identifier}, _From, State) ->
	case find_bucket_from_identifier(State, Identifier) of
		{ found , Bucket} ->
			case kvlists:get_value(Bucket, State) of
				undefined  -> 	
					Reply = { notfound, Identifier},
					State1 = State;
				[Bucket,{ monitors, MonitorList }]  ->
					MonitorList1 = MonitorList, % TODO : fix this
					State1 = kvlists:set_value(Bucket, [Bucket, {monitors , MonitorList1}], State),
					Reply = {acked, Identifier}
			end;
		{notfound} ->
			Reply = {notfound, Identifier},
			State1 = State
	end,

	%reliable_delivery_monitor_stats:decrement_persisted_monitors(),
	{reply, Reply ,State1};

handle_call({pop, Bucket}, _, State) ->
	case kvlists:get_value(Bucket, State) of
		undefined  ->
			Reply = {undefined};
		[Bucket,{monitors, MonitorList }] ->	
			Reply = {ok, MonitorList}
	end,
  	{reply,Reply,State};

handle_call({push, Bucket, OffsetInBucket, Identifier, LeaseTime, Application, Value}, _, State) ->
	
	case kvlists:get_value(Bucket, State) of
		undefined  ->
			State1 = [ [ Bucket , {monitors, [[{<<"inprogress">>, OffsetInBucket, Identifier, LeaseTime, Application, Value}]] }] | State];
		[Bucket,{monitors, MonitorList }] ->	
			MonitorList1 = [ [{<<"inprogress">>, OffsetInBucket, Identifier, LeaseTime, Application, Value}] | MonitorList ],
			State1 = kvlists:set_value(Bucket, [Bucket, {monitors , MonitorList1}], State)
	end,
	%reliable_delivery_monitor_stats:increment_persisted_monitors(),
  	{reply, ok ,State1};

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

find_bucket_from_identifier(_State, _Identifier) ->
	ok.