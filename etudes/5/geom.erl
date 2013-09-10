-module (geom).
-export ([area/1]).

-spec(area({atom(), number(),number()}) -> number()).
area({Type,A,B}) ->
	area(Type,A,B).

-spec(area(atom(), number(),number()) -> number()).
area(rectangle, A,B) when A > 0,
			   			  B > 0 ->
	{ok, A*B };
area(triangle, A,B) when A > 0,
			   			 B > 0 ->
	{ok, (A*B) /2 };
area(ellipse, A,B) when A > 0,
			   			B > 0 ->
	{ok,  math:pi() * A * B };
area(_,_,_) ->
	{error,0}.
