-module (powers_tail_recursive).
-export ([raise/2]).

% When N is greater than zero, raise/2 will call raise/3 with arguments X, N, and 1 as the Accumulator.
% The raise/3 function will return the Accumulator when N equals 0 (this will stop the recursion).
% Otherwise, recursively call raise/3 with X, N - 1, and X times the Accumulator as its arguments.
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
 