-module (non_fp).
-export ([generate_teeth/0,generate_teeth/2]).

-spec(generate_teeth() -> list(list(integer()))).
generate_teeth() ->
	generate_teeth("FTFTTTTTTTFT", random:uniform()).
	%generate_teeth("FTFTFTFTFTFTFTFTFTFTFTFTFTFTFTFTFTFT", random:uniform()).

-spec(generate_teeth(string(), float()) -> list(list(integer()))).
generate_teeth(Teeth,Random) ->
	io:format("Teeth : ~p, Random : ~p.~n", [Teeth,Random]),
	generate_teeth(Teeth,Random,[]).

generate_teeth([],_,Acc) ->
	lists:reverse(Acc);
generate_teeth([H|T],Random,Acc) ->
	case H of
		$T ->
			Tooth = generate_tooth(Random),
			generate_teeth(T,Random, [Tooth|Acc]);
		$F ->
			generate_teeth(T,Random, [[]|Acc])
	end.

generate_tooth(Random)->
  Good = random:uniform() < Random,
  case Good of
    true -> BaseDepth = 2;
    false -> BaseDepth = 3
  end,
  generate_tooth(BaseDepth,6,[]).

generate_tooth(_,0,Acc) ->
	Acc;
generate_tooth(BaseDepth,NumberOfDepths,Acc) ->
	[BaseDepth + random:uniform(3) - 2 | generate_tooth(BaseDepth, NumberOfDepths - 1, Acc)].
