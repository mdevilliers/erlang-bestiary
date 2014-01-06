-module (reliable_delivery).

-export ([start/0, monitor/3, ack/1, callback/3]).

-include ("reliable_delivery.hrl").

-type leaseTime() :: integer().
-type value() :: binary().
-type identifier() :: binary().

-spec start() -> ok.
start() ->
	lager:start(),
	application:start(folsom),
	application:start(crypto),
    application:start(ranch),
    application:start(cowboy),
    application:start(gproc),
	ok = application:start(reliable_delivery).

-spec monitor( LeaseTime ,Application, Value) -> {ok, Identifier} when 
	LeaseTime :: leaseTime(),
	Application :: binary(),
	Value :: value(),
	Identifier :: identifier().

monitor(LeaseTime, Application, Value) ->
	Identifier = reliable_delivery_uuid:generate(),
	reliable_delivery_bucket_store:push(Identifier, LeaseTime, Application, Value),

	reliable_delivery_monitor_stats:increment_total_monitors(),
    reliable_delivery_monitor_stats:increment_current_monitors(),
    
	{ok, Identifier}.

-spec ack(Identifier) -> {error, {identifier_not_found, Identifier }} | {already_acked,{ identifier, Identifier} } | {ok,{ identifier, Identifier}} when
	Identifier :: identifier().

ack(Identifier) ->
	Reply = reliable_delivery_bucket_store_lite:ack_with_identifier(Identifier),
	Reply.

-spec callback( unknown | expired, Identifier, Value | none) -> ok when
	Identifier :: identifier(),	
	Value :: value().
	
callback(unknown, Identifier, none) ->
	lager:info("Callback : ~p, ~p, ~p.~n", [already_expired , Identifier, none]),
	ok;
callback(expired, Identifier, _Value) ->
	lager:info("Callback : ~p, ~p, [Value not shown].~n", [expired , Identifier]),
	ok.