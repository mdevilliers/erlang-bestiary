-module (bucket_manager_tests).

-include_lib("eunit/include/eunit.hrl").
-include ("reliable_delivery.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).

start() ->
	reliable_delivery_bucket_manager:start_link().
stop(_) ->
	reliable_delivery_bucket_manager:stop().

running_application_test_() ->
	[{"get bucket offset",
		?setup( fun get_bucket_offset/0)},
	{"get current bucket",
		?setup( fun get_current_bucket/0)},
	{"bucket should increase by one when value == BucketDuration",
		?setup( fun bucket_value_equals_offset_duration/0)},
	{"current bucket should increase.",
		?setup( fun current_bucket_increments/0)}
	].

get_bucket_offset() ->

	BucketDuration = reliable_delivery_bucket_manager:get_bucket_duration(),

	Value = BucketDuration - 1, % ensure first bucket - 0
	{ bucket, Bucket, OffsetInBucket} = reliable_delivery_bucket_manager:get_bucket(Value),
	?assertEqual(Bucket, 0),
	?assertEqual(OffsetInBucket, Value),

	Value1 = BucketDuration + 1,
	{ bucket, Bucket1, OffsetInBucket1} = reliable_delivery_bucket_manager:get_bucket(Value1),
	?assertEqual(Bucket1, 1),
	?assertEqual(Bucket1, Bucket + 1),
	?assertEqual(OffsetInBucket1, 1).

get_current_bucket() ->

	CurrentBucket = reliable_delivery_bucket_manager:current_bucket(),

	{ bucket, Bucket, _} = reliable_delivery_bucket_manager:get_bucket(1),

	?assertEqual(Bucket, CurrentBucket).

bucket_value_equals_offset_duration() ->
	
	BucketDuration = reliable_delivery_bucket_manager:get_bucket_duration(),
	CurrentBucket = reliable_delivery_bucket_manager:current_bucket(),

	{ bucket, Bucket, _} = reliable_delivery_bucket_manager:get_bucket(BucketDuration),
	
	?assertEqual(Bucket, CurrentBucket + 1).

current_bucket_increments() ->
	
	BucketDuration = reliable_delivery_bucket_manager:get_bucket_duration(),
	CurrentBucket = reliable_delivery_bucket_manager:current_bucket(),

	timer:sleep(BucketDuration + 1),

	NextBucket = reliable_delivery_bucket_manager:current_bucket(),
	
	?assertEqual(NextBucket, CurrentBucket + 1).
