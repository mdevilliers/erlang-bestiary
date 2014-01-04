 -module (bucket_store_lite_tests).

-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).

start() ->
	?debugMsg(start),
	reliable_delivery_bucket_store_lite:start_link().
stop(_) ->
	?debugMsg(stop),
	reliable_delivery_bucket_store_lite:stop().

running_application_test_() ->
	[
		{"Append monitors to existing buckets.",?setup(fun append_to_existing_bucket/0)},
		{"Push multiple monitors to different buckets.",?setup(fun push_to_new_bucket/0)},
		{"Push to bucket and check push_to_and_pop_from_bucket.",?setup(fun push_to_bucket_and_ack/0)},
		{"Push to and pop from bucket.",?setup(fun push_to_and_pop_from_bucket/0)},
		{"Push to, ack and pop from bucket.",?setup(fun push_to_ack_and_pop_from_bucket/0)},
		{"Pop from non-existant bucket.",?setup(fun pop_from_non_existant_bucket/0)},
		{"Push to bucket and check state.",?setup(fun get_state_for_new_monitor/0)},
		{"Check state for non-existant monitor.",?setup(fun get_state_for_non_existant_monitor/0)},
		{"Check state for acked monitor.",?setup(fun get_state_for_acked_monitor/0)},
		{"Check state for existant bucket.",?setup(fun get_state_from_non_existant_bucket/0)}
	].

get_state_from_non_existant_bucket() ->
	 {error, not_found} = reliable_delivery_bucket_store_lite:get_state(1).

get_state_for_acked_monitor() ->
	Identifier = <<"abc">>,
	ok = reliable_delivery_bucket_store_lite:push_to_bucket( 1, 100, Identifier, 1000 ,<<"my application">>, <<"my value">>),
	ok = reliable_delivery_bucket_store_lite:ack_with_identifier(Identifier),
	{ok, MonitorState} = reliable_delivery_bucket_store_lite:get_state_for_monitor(Identifier),
	?assertEqual(MonitorState, <<"acked">> ).

get_state_for_new_monitor() ->
	Identifier = <<"abc">>,
	ok = reliable_delivery_bucket_store_lite:push_to_bucket( 1, 100, Identifier, 1000 ,<<"my application">>, <<"my value">>),
	{ok, MonitorState} = reliable_delivery_bucket_store_lite:get_state_for_monitor(Identifier),
	?assertEqual(MonitorState, <<"inprogress">> ).

get_state_for_non_existant_monitor() ->
	Identifier = <<"abc">>,
	{error, not_found} = reliable_delivery_bucket_store_lite:get_state_for_monitor(Identifier).

push_to_bucket_and_ack() ->
	Identifier = <<"abc">>,
	ok = reliable_delivery_bucket_store_lite:push_to_bucket( 1, 100, Identifier, 1000 ,<<"my application">>, <<"my value">>),
	ok = reliable_delivery_bucket_store_lite:push_to_bucket( 1, 100, <<"def">>, 1000 ,<<"my application">>, <<"my value">>),
	reliable_delivery_bucket_store_lite:ack_with_identifier(Identifier),
	{ok, State} = reliable_delivery_bucket_store_lite:get_state(1),
	?assertEqual(length(State),1).

pop_from_non_existant_bucket() ->
	 {error, not_found} = reliable_delivery_bucket_store_lite:pop_from_bucket(1).

push_to_ack_and_pop_from_bucket()->
	
	Identifier = <<"abc">>,
	ok = reliable_delivery_bucket_store_lite:push_to_bucket( 1, 100, Identifier, 1000 ,<<"my application">>, <<"my value">>),
	ok = reliable_delivery_bucket_store_lite:push_to_bucket( 1, 100, <<"def">>, 1000 ,<<"my application">>, <<"my value">>),
	reliable_delivery_bucket_store_lite:ack_with_identifier(Identifier),
	{ok, MonitorList} = reliable_delivery_bucket_store_lite:pop_from_bucket(1),
	%State = reliable_delivery_bucket_store_lite:get_state(1),
	?assertEqual(length(MonitorList),1).

push_to_and_pop_from_bucket()->

	ok = reliable_delivery_bucket_store_lite:push_to_bucket( 1, 100, <<"abc">>, 1000 ,<<"my application">>, <<"my value">>),
	ok = reliable_delivery_bucket_store_lite:push_to_bucket( 1, 100, <<"def">>, 1000 ,<<"my application">>, <<"my value">>),
	{ok, MonitorList} = reliable_delivery_bucket_store_lite:pop_from_bucket(1),
	%State = reliable_delivery_bucket_store_lite:get_state(1),
	?assertEqual(length(MonitorList),2).

push_to_new_bucket() ->

	ok = reliable_delivery_bucket_store_lite:push_to_bucket( 1, 100, <<"abc">>, 1000 ,<<"my application">>, <<"my value">>),
	ok = reliable_delivery_bucket_store_lite:push_to_bucket( 2, 100, <<"def">>, 1000 ,<<"my application">>, <<"my value">>),
	{ok, State} = reliable_delivery_bucket_store_lite:get_state(1),
	?assertEqual(length(State),1),
	{ok, State1} = reliable_delivery_bucket_store_lite:get_state(2),
	?assertEqual(length(State1),1).

append_to_existing_bucket() ->

	%?debugFmt("State before ~p ~n", [reliable_delivery_bucket_store_lite:get_state()]),
	Bucket = 1 ,
	ok = reliable_delivery_bucket_store_lite:push_to_bucket( Bucket, 100, <<"abc">>, 1000 ,<<"my application">>, <<"my value">>),
	ok = reliable_delivery_bucket_store_lite:push_to_bucket( Bucket, 100, <<"def">>, 1000 ,<<"my application">>, <<"my value">>),
	ok = reliable_delivery_bucket_store_lite:push_to_bucket( Bucket + 1, 100, <<"ghi">>, 1000 ,<<"my application">>, <<"my value">>),
	{ok, State} = reliable_delivery_bucket_store_lite:get_state(Bucket),
	?assertEqual(length(State),2),
	{ok, State1} = reliable_delivery_bucket_store_lite:get_state(Bucket + 1),
	?assertEqual(length(State1),1).

