-module (powers).
-export ([raise/2]).

% Any number to the power 0 equals 1.
% Any number to the power 1 is that number itself — that stops the recursion.
% When N is positive, XN is equal to X times X(N - 1) —  there’s your recursion.
% When N is negative, XN is equal to 1.0 / X-N
-spec(raise(number(), integer()) -> number()).
raise(_, 0) ->
	0;
raise(X, 1) ->
	X;
raise(X, N) when N > 0 ->
	X * raise(X, N-1);
raise(X, N) when N < 0 ->
	1.0 / raise(X , -N).

 