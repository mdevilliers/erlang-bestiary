-module (dates).
-export ([date_parts/1, julian/1]).

-spec(julian(string()) -> number()).
julian(DateStr) ->
	case date_parts(DateStr) of
		{Year,Month,Day} ->
			julian(Year,Month,Day,get_days_in_year(Year), 0);
		error ->
			error
	end.

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

%helpers
julian(_, 1, Day, _ , Acc) ->
	Acc+Day;
julian(_Year, Month, Day, [H|T], Acc) when Month < 13, Month > 0 ->
	julian(_Year, Month - 1, Day, T, Acc + H);
julian(_, _, _, _, _) ->
	check_your_months_value.

get_days_in_year(Year) ->
	case is_leap_year(Year) of
		true ->
			[31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
		false ->
			[31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	end.
	
is_leap_year(Year) ->
  	(Year rem 4 == 0 andalso Year rem 100 /= 0)
  	orelse (Year rem 400 == 0).
