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

	reliable_delivery_monitor_stats:increment_total_monitors(),
	reliable_delivery_monitor_stats:increment_current_monitors(),
	
	{ok, Identifier}.

-spec ack(Identifier) -> {error, {identifier_not_found, Identifier }} | {ok,{ identifier, Identifier}} when
	Identifier :: identifier().

ack(Identifier) ->
	case reliable_delivery_monitor_store:lookup(Identifier) of
		{error,not_found} ->
			reliable_delivery_monitor_stats:increment_unknown_monitors(),
			{error, {identifier_not_found, Identifier }};
		{ok, Pid, _ , _ , _} ->
			reliable_delivery_worker:notify_acked(Pid),
			reliable_delivery_monitor_stats:increment_acked_monitors(),
			{ok,{ identifier, Identifier} }
	end.

-spec callback( unknown | expired, Identifier, Value | none) -> ok when
	Identifier :: identifier(),	
	Value :: value().
	
callback(unknown, Identifier, none) ->
	reliable_delivery_monitor_stats:increment_unknown_monitors(),
	lager:info("Callback : ~p, ~p, ~p.~n", [already_expired , Identifier, none]),
	ok;
callback(expired, Identifier, _Value) ->
	reliable_delivery_monitor_stats:increment_expired_monitors(),
	lager:info("Callback : ~p, ~p, [Value not shown].~n", [expired , Identifier]),
	ok.