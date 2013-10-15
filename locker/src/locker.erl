-module (locker).
-export ([start/0]).


start() ->
	lager:start(),
	application:start(folsom),
	application:start(locks),
	application:start(locker).