-module (stats).
-export ([minimum/1]).

-spec(minimum(list(number())) -> number()).
minimum([H|T]) ->
	minimum(H,T).


minimum(Smallest, []) ->
	Smallest;
minimum(Smallest,[H|T]) ->
	if
		H < Smallest ->
			minimum(H, T);
		H > Smallest ->
			minimum(Smallest,T)
	end.