-module (reliable_delivery).

-export ([start/0, monitor/3, ack/1, callback/3]).

start() ->
	lager:start(),
	application:start(reliable_delivery).

monitor(Identifier, LeaseTime, Value) ->
	{ok,Pid} = reliable_delivery_sup:start_monitor(Identifier, LeaseTime),
	message_store:insert(Identifier, Pid, Value),
	{info, ok}.

ack(Identifier) ->
	case message_store:lookup(Identifier) of
		{error,not_found} ->
			{info, key_not_found};
		{ok, Pid, _} ->
			reliable_delivery_worker:notify_acked(Pid),
			{info, ok}
	end.

callback(expired, Identifier, Value) ->
	lager:info("Callback : ~p, ~p, ~p.~n", [expired , Identifier, Value]),
	ok.