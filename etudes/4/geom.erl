-module (geom).
-export ([area/1]).

-spec(area({atom(), number(),number()}) -> number()).
area({Type,A,B}) ->
	area(Type,A,B).

-spec(area(atom(), number(),number()) -> number()).
area(Type, A,B) when A > 0,
			   		 B > 0 ->
	case Type of
		rectangle ->
			{ok, A*B };
		triangle ->
			{ok, (A*B) /2 };
		ellipse ->
		    {ok,  math:pi() * A * B };
		_ -> {error,0}

	end;
area(_,_,_) ->
	{error,0}.
