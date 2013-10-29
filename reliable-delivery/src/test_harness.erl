-module (test_harness).

-export ([start/1]).
-export ([do_start/1, do_ack/2]).

start(0) ->
	ok;
start(Iterations) ->
	spawn(?MODULE, do_start, [500]),
	start(Iterations-1).

do_start(0) ->
	ok;
do_start(Number) ->
	Random = random:uniform(1000000),
	{ok, Identifier} = reliable_delivery:monitor(Random , pad_to(125, <<"data">>)),

	case mod(Random,2) of
		0  ->
			spawn(?MODULE, do_ack, [Identifier, Random]),
			do_start(Number - 1);
		1 ->
			do_start(Number - 1)
	end.

do_ack(Identifier, Timeout) ->
	timer:sleep(random:uniform(Timeout)),
	reliable_delivery:ack(Identifier).

mod(X,Y)->(X rem Y + Y) rem Y. 

% helpers
pad_to(Width, Binary) ->
     case (Width - size(Binary)) > 0 
       of false -> Binary;
		  true -> pad_to(Width,<<Binary/binary, Binary/binary>>)
     end.