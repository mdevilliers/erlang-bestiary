-module(euler_one).
-export([euler/1,euler/0]).

% If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
% Find the sum of all the multiples of 3 or 5 below 1000.

euler() ->
	euler(1000).
euler(N) ->
	solve(N - 1,0).

solve(0,Acc) ->
	Acc;
solve(N,Acc) ->
	if
		N rem 3 == 0 ->
			solve(N-1, Acc + N);
		N rem 5 == 0 ->
			solve(N-1, Acc + N);
		true ->
			solve(N-1, Acc)
	end.
