-module (reliable_delivery).

-export ([start/0, monitor/3, ack/1, callback/3]).

start() ->
	application:start(reliable_delivery).

monitor(Identifier, LeaseTime, Value) ->
	{ok,Pid} = reliable_delivery_sup:start_monitor(Identifier, LeaseTime),
	message_store:insert(Identifier, Pid, Value),
	Identifier.

ack(Identifier) ->
	{ok, Pid, _} = message_store:lookup(Identifier),
	reliable_delivery_worker:notify_acked(Pid),
	ok.

callback(expired, Identifier, Value) ->
	io:format("Callback : ~p, ~p, ~p.~n", [expired,Identifier,Value]),
	ok.