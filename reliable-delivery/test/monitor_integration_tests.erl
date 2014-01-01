-module (monitor_integration_tests).

-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).
-define(foreach(Tests), {foreach, fun start/0, Tests}).

start() ->
	reliable_delivery:start().
stop(_) ->
	application:stop(reliable_delivery).

running_application_test_() ->
	[{"Monitor integration tests.",
		?setup(
			[
			fun monitor_then_wait_then_ack_too_late/0,
			fun monitor_then_wait_then_ack_and_ack_again/0,
			fun monitor_ack_unknown_monitor/0,
			fun monitor_then_wait_for_expire_with_short_expiry_time/0,
			fun monitor_then_ack_with_short_expiry_time/0
			])}].

monitor_then_ack_with_short_expiry_time() ->
    
    reliable_delivery_monitor_stats:reset_all_metrics(),

    ExpiryTime = 10,

	{ok, Identifier} = reliable_delivery:monitor(ExpiryTime,<<"my application name">>, <<"my value">>),
	{ok,{ identifier, Identifier}} = reliable_delivery:ack(Identifier),

	Stats = get_current_stats(),
	
	ExpiredItems = kvlists:get_value("monitored_items_expired" , Stats),
	TotalItems = kvlists:get_value("monitored_items_total" , Stats),
	AckedItmes =  kvlists:get_value("monitored_items_acked" , Stats),

	?assertEqual(AckedItmes, TotalItems),
	?assertEqual(ExpiredItems, 0),
	?assertEqual(TotalItems, 1),
	?assertEqual(AckedItmes, 1).

monitor_then_wait_for_expire_with_short_expiry_time() ->
    
    reliable_delivery_monitor_stats:reset_all_metrics(),

    ExpiryTime = 10,

	{ok, Identifier} = reliable_delivery:monitor(ExpiryTime,<<"my application name">>, <<"my value">>),

	timer:sleep(ExpiryTime + ExpiryTime),
	
	{error, {identifier_not_found, Identifier }} = reliable_delivery:ack(Identifier),

	Stats = get_current_stats(),
	
	ExpiredItems = kvlists:get_value("monitored_items_expired" , Stats),
	TotalItems = kvlists:get_value("monitored_items_total" , Stats),
	AckedItmes =  kvlists:get_value("monitored_items_acked" , Stats),

	?assertEqual(ExpiredItems, TotalItems),
	?assertEqual(ExpiredItems, 1),
	?assertEqual(TotalItems, 1),
	?assertEqual(AckedItmes, 0).

monitor_then_wait_then_ack_too_late() ->
    
    reliable_delivery_monitor_stats:reset_all_metrics(),

    BucketDuration = reliable_delivery_bucket_manager:get_bucket_duration(),

	{ok, Identifier} =  reliable_delivery:monitor(BucketDuration,<<"my application name">>, <<"my value">>),

	timer:sleep(BucketDuration + BucketDuration),
	
	{error, {identifier_not_found, Identifier }} = reliable_delivery:ack(Identifier),

	Stats = get_current_stats(),
	
	ExpiredItems = kvlists:get_value("monitored_items_expired" , Stats),
	TotalItems = kvlists:get_value("monitored_items_total" , Stats),
	AckedItmes =  kvlists:get_value("monitored_items_acked" , Stats),

	?assertEqual(ExpiredItems, TotalItems),
	?assertEqual(ExpiredItems, 1),
	?assertEqual(TotalItems, 1),
	?assertEqual(AckedItmes, 0).

monitor_then_wait_then_ack_and_ack_again() ->
    
	reliable_delivery_monitor_stats:reset_all_metrics(),

	{ok, Identifier} = reliable_delivery:monitor(5000,<<"my application name">>, <<"my value">>),

	timer:sleep(100),

	{ok,{ identifier, Identifier}} = reliable_delivery:ack(Identifier),

	% attempt double ack
	{already_acked,{ identifier, Identifier} } = reliable_delivery:ack(Identifier),

	Stats = get_current_stats(),
	
	AckedItems =  kvlists:get_value("monitored_items_acked" , Stats),
	TotalItems = kvlists:get_value("monitored_items_total" , Stats),

	?assertEqual(AckedItems, TotalItems),
	?assertEqual(AckedItems, 1),
	?assertEqual(TotalItems, 1).

monitor_ack_unknown_monitor() ->
    
    reliable_delivery_monitor_stats:reset_all_metrics(),

	Identifier = <<"rubbish">>,
	{error, {identifier_not_found, Identifier }} = reliable_delivery:ack(Identifier),

	Stats = get_current_stats(),
	
	UnknownItems =  kvlists:get_value("monitored_items_unknown" , Stats),
	?assertEqual(UnknownItems, 1).
 
%% helpers

get_current_stats() ->
	Stats = reliable_delivery_monitor_stats:all_metrics_with_values(),
	stats_as_flat_kv(Stats).

stats_as_flat_kv(Stats) ->
	flatten_stats(Stats, []).

flatten_stats([], Acc) ->
	Acc;
flatten_stats([ [{ <<"name">>, _ },{ <<"identifier">>, Name }, { <<"value">>, Value},{ <<"description">>, _}] |T], Acc) ->
	flatten_stats(T, [{Name,Value} | Acc]).