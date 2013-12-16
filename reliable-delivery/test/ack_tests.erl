-module (ack_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
 
monitor_then_wait_then_ack_test() ->
    
	reliable_delivery:start(),
	
	{ok, Identifier} =  reliable_delivery:monitor(5000,<<"my application name">>, <<"my value">>),

	timer:sleep(100),

	{ok,{ identifier, Identifier}} = reliable_delivery:ack(Identifier),

	% attempt double ack
	{already_acked,{ identifier, Identifier} } = reliable_delivery:ack(Identifier),

	application:stop(reliable_delivery).
 
-endif.