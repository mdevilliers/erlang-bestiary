-module (riak_ensemble_test).

-export ([start/0]).

start()->
	lager:start(),
	ok = application:start(riak_ensemble_test).