-module (phone_ets).
-export ([setup/1, summary/1, summary/0]).

-include ("phone_records.hrl").

setup(FileName) ->
	%% If the table exists, delete it
  	case ets:info(call_table) of
   		undefined -> false;
    	_ -> ets:delete(call_table)
   end,

  	%% and create it anew
  	ets:new(call_table, [named_table, bag, {keypos, #phone_call.number}]),

	{Result, InputFile} = file:open(FileName, [read]),
	case Result of
	    ok -> read_lines(InputFile);
	    _ -> io:format("Error opening file: ~p~n", [FileName])
	  end.

summary() ->
  FirstKey = ets:first(call_table),
  summary(FirstKey, []).

summary(Key, Result) ->
  NextKey = ets:next(call_table, Key),
  case NextKey of
    '$end_of_table' -> Result;
    _ -> summary(NextKey, [hd(summary(Key)) | Result])
  end.

summary(PhoneNumber) ->
  Calls = ets:lookup(call_table, PhoneNumber),
  Total = lists:foldl(fun subtotal/2, 0, Calls),
  [{PhoneNumber, Total}].

% helpers
subtotal(Item, Accumulator) ->
  % bleurghhhhh
  StartDate = {(Item#phone_call.call_start_date)#date.year,(Item#phone_call.call_start_date)#date.month,(Item#phone_call.call_start_date)#date.day},
  StartTime = {(Item#phone_call.call_start_time)#time.hour,(Item#phone_call.call_start_time)#time.minute,(Item#phone_call.call_start_time)#time.second},
  EndDate = {(Item#phone_call.call_end_date)#date.year,(Item#phone_call.call_end_date)#date.month,(Item#phone_call.call_end_date)#date.day},
  EndTime =  {(Item#phone_call.call_end_time)#time.hour,(Item#phone_call.call_end_time)#time.minute,(Item#phone_call.call_end_time)#time.second},

  StartSeconds = calendar:datetime_to_gregorian_seconds(
    {StartDate, StartTime}),
  EndSeconds = calendar:datetime_to_gregorian_seconds(
    {EndDate,EndTime}),
  Accumulator + ((EndSeconds - StartSeconds + 59) div 60).

read_lines(InputFile) ->
	Line = io:get_line(InputFile, ""),
	case Line of
		eof  ->
			ok;
		_ ->
			 ets:insert(call_table, parse_line(Line)), 
			 read_lines(InputFile)
	end.

parse_line(Line) ->
	case re:split(Line, "[,]", [{return,list}]) of
		[PhoneNumber,StartDate,StartTime,EndDate,EndTime]  ->
			#phone_call{number = PhoneNumber,  call_start_date = parse_date(StartDate), call_start_time = parse_time(StartTime) , call_end_date = parse_date(EndDate), call_end_time = parse_time(EndTime)};
		_ -> error
	end.

parse_date(Date) ->
	case re:split(Date, "[-]", [{return,list}]) of
		[Year,Month,Day]  ->
			{Y,_} = string:to_integer(Year),
			{M,_} = string:to_integer(Month),
			{D,_} =  string:to_integer(Day),
			#date{year = Y, month = M, day = D};
		_ -> error
	end.

parse_time(Time) ->
	case re:split(Time, "[:]", [{return,list}]) of
		[Hour,Minute,Second]  ->
			{H,_} = string:to_integer(Hour),
			{M,_} = string:to_integer(Minute),
			{S,_} =  string:to_integer(Second),
			#time{hour = H, minute = M, second = S};
		_ -> error
	end.