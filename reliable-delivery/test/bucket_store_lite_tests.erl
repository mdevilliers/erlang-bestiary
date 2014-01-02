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
		{"Push multiple monitors to different buckets.",?setup(fun push_non_existant_bucket/0)},
		{"Add to and pop from bucket.",?setup(fun push_to_and_pop_from_bucket/0)},
		{"Add multiple to and pop from bucket.",?setup(fun push_mulitple_to_and_pop_from_bucket/0)}
	].

push_mulitple_to_and_pop_from_bucket()->
	%?debugFmt("State before ~p ~n", [reliable_delivery_bucket_store_lite:get_state()]),
	ok = reliable_delivery_bucket_store_lite:push_to_bucket( 1, 100, <<"abc">>, 1000 ,<<"my application">>, <<"my value">>),
	ok = reliable_delivery_bucket_store_lite:push_to_bucket( 1, 100, <<"def">>, 1000 ,<<"my application">>, <<"my value">>),
	{ok, MonitorList} = reliable_delivery_bucket_store_lite:pop_from_bucket(1),
	%State = reliable_delivery_bucket_store_lite:get_state(),
	%?debugFmt("State ~p ~n Monitor ~p ~n", [State, MonitorList]),
	?assertEqual(length(MonitorList),2).

push_to_and_pop_from_bucket()->
	%?debugFmt("State before ~p ~n", [reliable_delivery_bucket_store_lite:get_state()]),
	ok = reliable_delivery_bucket_store_lite:push_to_bucket( 1, 100, <<"abc">>, 1000 ,<<"my application">>, <<"my value">>),
	{ok, MonitorList} = reliable_delivery_bucket_store_lite:pop_from_bucket(1),
	%State = reliable_delivery_bucket_store_lite:get_state(),
	%?debugFmt("State ~p ~n Monitor ~p ~n", [State, MonitorList]),
	?assertEqual(length(MonitorList),1).

push_non_existant_bucket() ->

	%?debugFmt("State before ~p ~n", [reliable_delivery_bucket_store_lite:get_state()]),
	ok = reliable_delivery_bucket_store_lite:push_to_bucket( 1, 100, <<"abc">>, 1000 ,<<"my application">>, <<"my value">>),
	ok = reliable_delivery_bucket_store_lite:push_to_bucket( 2, 100, <<"def">>, 1000 ,<<"my application">>, <<"my value">>),
	State = reliable_delivery_bucket_store_lite:get_state(),
	%?debugFmt("State ~p ~n", [State]),
	?assertEqual(length(State),2).

append_to_existing_bucket() ->

	%?debugFmt("State before ~p ~n", [reliable_delivery_bucket_store_lite:get_state()]),
	Bucket = 1 ,
	ok = reliable_delivery_bucket_store_lite:push_to_bucket( Bucket, 100, <<"abc">>, 1000 ,<<"my application">>, <<"my value">>),
	ok = reliable_delivery_bucket_store_lite:push_to_bucket( Bucket, 100, <<"def">>, 1000 ,<<"my application">>, <<"my value">>),
	ok = reliable_delivery_bucket_store_lite:push_to_bucket( Bucket + 1, 100, <<"ghi">>, 1000 ,<<"my application">>, <<"my value">>),
	State = reliable_delivery_bucket_store_lite:get_state(),
	%?debugFmt("~n State ~p ~n", [State]),
	?assertEqual(length(State),2).

