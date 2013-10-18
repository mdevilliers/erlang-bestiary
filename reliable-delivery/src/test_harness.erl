-module (test_harness).

-export ([start/0]).
-export ([do_start/1]).

start() ->
	spawn(?MODULE, do_start, [500]),
	spawn(?MODULE, do_start, [500]),
	spawn(?MODULE, do_start, [500]),
	ok.

do_start(0) ->
	ok;
do_start(Number) ->
	Random = random:uniform(10000),
	B1= list_to_binary(integer_to_list(Random)),
	B2= <<"identifier">>,
	reliable_delivery:monitor(<<B1/binary, B2/binary>>, Random * 100, pad_to(125, <<"data">>)),
	do_start(Number-1).

pad_to(Width, Binary) ->
     case (Width - size(Binary)) > 0 
       of false -> Binary;
		  true -> pad_to(Width,<<Binary/binary, Binary/binary>>)
     end.