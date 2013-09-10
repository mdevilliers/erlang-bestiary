-module (dates).
-export ([date_parts/1]).

-spec(date_parts(string()) -> {integer(),integer(),integer()}).
date_parts(DateStr) ->
	case re:split(DateStr, "[-]", [{return,list}]) of
		[Year,Month,Day]  ->
			{YearInt, _} = string:to_integer(Year),
			{MonthInt, _} = string:to_integer(Month),
			{DayInt, _} = string:to_integer(Day),
			{YearInt,MonthInt,DayInt};
		_ -> error
	end.