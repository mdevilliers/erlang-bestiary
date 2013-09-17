-module (stats).
-export ([minimum/1, maximum/1, range/1, mean/1, stdv/1]).

-spec(mean(list(number())) -> number()).
mean(List) ->
	Sum = lists:foldl(fun(X, S) -> X + S end, 0, List),
	Sum / length(List).

-spec(stdv(list(number())) -> number()).
stdv(List) ->
	Sum = lists:foldl(fun(X, S) -> X + S end, 0, List),
	SumOfSquares =  lists:foldl(fun(X, S) -> S + (X * X) end, 0, List),
	Length = length(List),
	math:sqrt(( Length * SumOfSquares - Sum * Sum)/(Length * (Length - 1))).

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