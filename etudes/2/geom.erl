-module (geom).
-export ([area/2]).

-spec(area(number(),number()) -> number()).

area(L,H) when is_number(L),
			   is_number(H),
			   L > 0,
			   H > 0 ->
	{ok, L*H };
area(_,_) ->
	{error}.