-module (ack_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
 
-define(setup(F), {setup, fun start/0, fun stop/1, F}).
-define(foreach(Tests), {foreach, fun start/0, Tests}).

start() ->
	reliable_delivery:start().
stop(_) ->
	application:stop(reliable_delivery).

running_application_test_() ->
	[{"Tests requiring running reliable_delivery application",
		?setup(
			[
			fun monitor_then_wait_then_ack_and_ack_again/0,
			fun monitor_ack_unknown_monitor/0
			])}].

monitor_then_wait_then_ack_and_ack_again() ->
    
	{ok, Identifier} =  reliable_delivery:monitor(5000,<<"my application name">>, <<"my value">>),

	timer:sleep(100),

	{ok,{ identifier, Identifier}} = reliable_delivery:ack(Identifier),

	% attempt double ack
	{already_acked,{ identifier, Identifier} } = reliable_delivery:ack(Identifier).

monitor_ack_unknown_monitor() ->
    
	Identifier = <<"rubbish">>,
	{error, {identifier_not_found, Identifier }} = reliable_delivery:ack(Identifier).
 
-endif.