-module (test_harness).

-export ([start/1]).
-export ([do_start/1, do_ack/2]).

-define (MIN_TIME, 100).

start(0) ->
	ok;
start(Iterations) ->
	random:seed(now()),
	spawn(?MODULE, do_start, [500]),
	start(Iterations-1).

do_start(0) ->
	ok;
do_start(Number) ->
	random:seed(now()),
	Random = random:uniform(60 * 1000) + ?MIN_TIME, % 60 seconds max
	{ok, Identifier} = reliable_delivery:monitor(Random , pad_to(1024, <<"data">>)),

	case mod(Random,2) of
		0  ->
			spawn(?MODULE, do_ack, [Identifier, Random]),
			do_start(Number - 1);
		1 ->
			do_start(Number - 1)
	end.

do_ack(Identifier, Timeout) ->
	timer:sleep(Timeout - ?MIN_TIME),
	reliable_delivery:ack(Identifier).

mod(X,Y)->(X rem Y + Y) rem Y. 

% helpers
pad_to(Width, Binary) ->
     case (Width - size(Binary)) > 0 
       of false -> Binary;
		  true -> pad_to(Width,<<Binary/binary, Binary/binary>>)
     end.