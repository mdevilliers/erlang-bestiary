-module (uses_a_behaviour).

-behaviour (defines_a_behaviour).

-export ([init/1, handle/1]).

init(_Args) ->
	ok.

handle(_Args) ->
	ok.