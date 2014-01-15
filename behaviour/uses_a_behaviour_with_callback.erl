-module (uses_a_behaviour_with_callback).
-behaviour (defines_a_behaviour_with_callback).

-export ([init/1, handle/1]).

init(_Args) ->
	ok.
	
handle(_Args) ->
	ok.