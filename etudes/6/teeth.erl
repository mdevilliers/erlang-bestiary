-module (teeth).
-export ([alert/0,alert/1]).

-spec(alert() -> [integer()]).
alert() ->
	PocketDepths = [[0], [2,2,1,2,2,1], [3,1,2,3,2,3],
						[3,1,3,2,1,2], [3,2,3,2,2,1], [2,3,1,2,1,1],
						[3,1,3,2,3,2], [3,3,2,1,3,1], [4,3,3,2,3,3],
						[3,1,1,3,2,2], [4,3,4,3,2,3], [2,3,1,3,2,2],
						[1,2,1,1,3,2], [1,2,2,3,2,3], [1,3,2,1,3,3], [0],
						[3,2,3,1,1,2], [2,2,1,1,3,2], [2,1,1,1,1,2],
						[3,3,2,1,1,3], [3,1,3,2,3,2], [3,3,1,2,3,3],
						[1,2,2,3,3,3], [2,2,3,2,3,3], [2,2,2,4,3,4],
						[3,4,3,3,3,4], [1,1,2,3,1,2], [2,2,3,2,1,3],
						[3,4,2,4,4,3], [3,3,2,1,2,3], [2,2,2,2,3,3],
						[3,2,3,2,3,2]],
	alert(PocketDepths).


-spec(alert([integer()]) -> [integer()]).
alert(PocketDepths) ->
	alert(PocketDepths,[], 0).

alert([], Acc, _) ->
	lists:reverse(Acc);
alert([Head|Tail], Acc, CurrentTooth) ->
	case tooth(Head) of
		true ->
			alert(Tail, [CurrentTooth + 1|Acc], CurrentTooth + 1);
		false ->
			alert(Tail, Acc, CurrentTooth + 1)
	end.

tooth([]) ->
	false;
tooth([Head|Tail])->
	if
		Head > 3 ->
			true;
		true ->
			tooth(Tail)
	end.
