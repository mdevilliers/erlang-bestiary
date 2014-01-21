-module (nif_tests).

-include_lib("eunit/include/eunit.hrl").
-define(setup(F), {setup, fun start/0, fun stop/1, F}).

start() ->
	ok.
stop(_) ->
	ok.

run_nif_test_() ->
	[{"call c from nif",
		?setup( fun call_c_from_nif/0)},
	{"call c that calls c++ from nif",
		?setup( fun call_c_that_calls_cpp_from_nif/0)}
	].

call_c_from_nif() ->
	hello = nif_example:hello_from_c().

call_c_that_calls_cpp_from_nif() ->
	1 = nif_example:hello_from_cpp().