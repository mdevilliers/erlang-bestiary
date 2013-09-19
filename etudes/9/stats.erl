-module (stats).
-export ([minimum/1, maximum/1, range/1, mean/1, stdv/1]).

-spec(mean(list(number())) -> number()).
mean(List) ->
	try
		Sum = lists:foldl(fun(X, S) -> X + S end, 0, List),
		Sum / length(List)
	 catch
    	error:Error -> {error, Error}
  	end.

-spec(stdv(list(number())) -> number()).
stdv(List) ->
	try
		Sum = lists:foldl(fun(X, S) -> X + S end, 0, List),
		SumOfSquares =  lists:foldl(fun(X, S) -> S + (X * X) end, 0, List),
		Length = length(List),
		math:sqrt(( Length * SumOfSquares - Sum * Sum)/(Length * (Length - 1)))
	 catch
    	error:Error -> {error, Error}
  	end.

-spec(minimum(list(number())) -> number()).
minimum(NumberList) ->
	try minimum(hd(NumberList),NumberList)
	catch
		error:Error -> {error, Error}
	end.

-spec(maximum(list(number())) -> number()).
maximum(NumberList) ->
	try maximum(hd(NumberList),NumberList)
	catch
    	error:Error -> {error, Error}
  	end.

-spec(range(list(number())) -> [number()]).
range(NumberList) ->
	try [minimum(hd(NumberList),NumberList),maximum(hd(NumberList),NumberList)]
	catch
		error:Error -> {error, Error}
  	end.	

%helpers
maximum(Largest, []) ->
	Largest;
maximum(Largest,[H|T]) ->
	case Largest > H of
		true -> maximum(Largest,T);
		false -> maximum(H,T)
	end.

minimum(Smallest, []) ->
	Smallest;
minimum(Smallest,[H|T]) ->
	case Smallest < H of
		true -> minimum(Smallest,T);
		false -> minimum(H,T)
	end.