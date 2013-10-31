-module (reliable_delivery).

-export ([start/0, monitor/2, ack/1, callback/3]).

-type leaseTime() :: integer().
-type value() :: binary().
-type identifier() :: binary().

-spec start() -> ok.
start() ->
	lager:start(),
	ok = application:start(folsom),
	ok = application:start(crypto),
    ok = application:start(ranch),
    ok = application:start(cowboy),
	ok = application:start(reliable_delivery).

-spec monitor( LeaseTime , Value) -> {ok, Identifier} when 
	LeaseTime :: leaseTime(),
	Value :: value(),
	Identifier :: identifier().

monitor(LeaseTime, Value) ->
	Identifier = reliable_delivery_uuid:binary( reliable_delivery_uuid:gen(), "monitor" ),
	{ok,Pid} = reliable_delivery_monitor_sup:start_monitor(Identifier, LeaseTime),
	reliable_delivery_monitor_store:insert(Identifier, Pid, Value, LeaseTime),
	folsom_metrics:new_counter(monitored_items_total),
	folsom_metrics:new_counter(monitored_items_current),
	folsom_metrics:notify({monitored_items_total, {inc, 1}}),
	folsom_metrics:notify({monitored_items_current, {inc, 1}}),
	{ok, Identifier}.

-spec ack(Identifier) -> {error,key_not_found} | {ok,info} when
	Identifier :: identifier().

ack(Identifier) ->
	case reliable_delivery_monitor_store:lookup(Identifier) of
		{error,not_found} ->
			folsom_metrics:new_counter(monitored_items_unknown),
	        folsom_metrics:notify({monitored_items_unknown, {inc, 1}}),
			{error, key_not_found};
		{ok, Pid, _ , _ , _} ->
			reliable_delivery_worker:notify_acked(Pid),
			folsom_metrics:new_counter(monitored_items_acked),
	        folsom_metrics:notify({monitored_items_acked, {inc, 1}}),
			{ok, info}
	end.

-spec callback( already_expired | expired, Identifier, Value | none) -> ok when
	Identifier :: identifier(),	
	Value :: value().
	
callback(already_expired, Identifier, none) ->
	folsom_metrics:new_counter(monitored_items_already_expired),
	folsom_metrics:notify({monitored_items_already_expired, {inc, 1}}),
	lager:info("Callback : ~p, ~p, ~p.~n", [already_expired , Identifier, none]),
	ok;
callback(expired, Identifier, Value) ->
	folsom_metrics:new_counter(monitored_items_expired),
	folsom_metrics:notify({monitored_items_expired, {inc, 1}}),
	lager:info("Callback : ~p, ~p, ~p.~n", [expired , Identifier, Value]),
	ok.