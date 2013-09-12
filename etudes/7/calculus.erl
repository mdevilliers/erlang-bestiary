-module (calculus).

-export ([derivative/2]).

derivative(Func, X) ->
		Delta = 1.0e-10,
        (Func(X + Delta) - Func(X)) / Delta.


