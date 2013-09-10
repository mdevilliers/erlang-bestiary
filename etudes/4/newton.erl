-module (newton).
-export ([nth_root/2]).

-spec(nth_root(number(), integer()) -> number()).

nth_root(X, N) -> nth_root(X, N, X / 2.0).


nth_root(X, N, A) ->	
	io:format("Current guess is ~p.~n", [A]),
	
	F = raise(A,N) - X,
	FPrime = N * raise(A, N - 1),
	Next = A - F / FPrime,
	Change = abs(Next - A), 
	NearEnough = 1.0e-8, % 1.0000000000

	if
		Change < NearEnough ->
			Next;
		true ->
			nth_root(X, N, Next)
	end.

-spec(raise(number(), integer()) -> number()).

raise(_, 0) ->
	0;
raise(X,N) when N > 0 ->
	raise(1, X, N);
raise(X, N) when N < 0 ->
	1.0 / raise(X , -N).

raise(Acc, _, 0) ->
	Acc;
raise(Acc, X, N) ->
	raise(X*Acc, X, N-1).