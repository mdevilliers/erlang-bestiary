-module (dijkstra).
-export ([gcd/2]).

% To find the GCD of integers M and N:
%  If M and N are equal, the result is M.
%  If M is greater than N, the result is the GCD of M - N and N
%  Otherwise M must be less than N, and the result is the GCD of M and N - M.

-spec(gcd(number(),number()) -> number()).

gcd(M,N) when M =:= N, M > 0, N > 0 ->
	M;
gcd(M,N) when M > N, M > 0, N > 0 ->
	gcd(M - N, M);
gcd(M,N) when M < N, M > 0, N > 0 ->
	gcd(M , N- M).
