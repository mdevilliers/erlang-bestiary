-module (reliable_delivery).

-export ([start/0, monitor/3, ack/1, callback/3]).

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
    ok = application:start(gproc),
	ok = application:start(reliable_delivery).

-spec monitor( LeaseTime ,Application, Value) -> {ok, Identifier} when 
	LeaseTime :: leaseTime(),
	Application :: binary(),
	Value :: value(),
	Identifier :: identifier().

monitor(LeaseTime,Application, Value) ->
	Identifier = reliable_delivery_uuid:generate(),
	{ok,_} = reliable_delivery_monitor_sup:start_monitor(Identifier, LeaseTime, Application, Value),
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
	lager:info("Callback : ~p, ~p, ~p.~n", [already_expired , Identifier, none]),
	ok;
callback(expired, Identifier, _Value) ->
	lager:info("Callback : ~p, ~p, [Value not shown].~n", [expired , Identifier]),
	ok.