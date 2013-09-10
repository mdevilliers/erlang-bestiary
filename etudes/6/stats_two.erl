-module (stats_two).
-export ([minimum/1, maximum/1, range/1]).

-spec(minimum(list(number())) -> number()).
minimum([H|T]) ->
	minimum(H,T).

-spec(maximum(list(number())) -> number()).
maximum([H|T]) ->
	maximum(H,T).

-spec(range(list(number())) -> [number()]).
range([H|T]) ->
	Min = minimum(H,T),
	Max = maximum(H,T),
	[Min,Max].


maximum(Largest, []) ->
	Largest;
maximum(Largest,[H|T]) ->
	if
		H > Largest ->
			maximum(H, T);
		H < Largest ->
			maximum(Largest,T)
	end.


minimum(Smallest, []) ->
	Smallest;
minimum(Smallest,[H|T]) ->
	if
		H < Smallest ->
			minimum(H, T);
		H > Smallest ->
			minimum(Smallest,T)
	end.