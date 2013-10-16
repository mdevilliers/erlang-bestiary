-module (reliable_delivery).

-export ([start/0, monitor/3, ack/1, callback/3,start_test_harness/0]).
-export ([do_start_test_harness/1]).

start() ->
	lager:start(),
	ok = application:start(folsom),
	ok = application:start(crypto),
    ok = application:start(ranch),
    ok = application:start(cowboy),
	application:start(reliable_delivery).

start_test_harness() ->
	reliable_delivery:start(),
	spawn(?MODULE, do_start_test_harness, [500]),
	spawn(?MODULE, do_start_test_harness, [500]),
	spawn(?MODULE, do_start_test_harness, [500]),
	ok.

do_start_test_harness(0) ->
	ok;
do_start_test_harness(Number) ->
	B1= list_to_binary(integer_to_list(Number)),
	B2= <<"identifier">>,
	reliable_delivery:monitor(<<B1/binary, B2/binary>>, Number * 1000, <<"data">>),
	do_start_test_harness(Number-1).

monitor(Identifier, LeaseTime, Value) ->
	{ok,Pid} = reliable_delivery_sup:start_monitor(Identifier, LeaseTime),
	message_store:insert(Identifier, Pid, Value),
	folsom_metrics:new_counter(monitored_items),
	folsom_metrics:notify({monitored_items, {inc, 1}}),
	{info, ok}.

ack(Identifier) ->
	case message_store:lookup(Identifier) of
		{error,not_found} ->
			folsom_metrics:new_counter(monitored_items_missed_ack),
	        folsom_metrics:notify({monitored_items_missed_ack, {inc, 1}}),
			{info, key_not_found};
		{ok, Pid, _} ->
			reliable_delivery_worker:notify_acked(Pid),
			folsom_metrics:new_counter(monitored_items_acked),
	        folsom_metrics:notify({monitored_items_acked, {inc, 1}}),
			{info, ok}
	end.

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