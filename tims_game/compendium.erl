-module (compendium).

-export ([generate_compendium/0]).


generate_compendium() ->
	do_generate_compendium(15).

% helpers
do_generate_compendium(Number) when Number < 1 ->
	ok;
do_generate_compendium(Number) ->
	Board = set:new_board(Number, Number),
	Json = set:game_as_json(Board),
	io:format("Hash ~p : ~p.~n",[Number, erlang:phash2(Json)]),
	json_to_file("game_" ++ integer_to_list(Number) ++ ".json", Json, false),
	do_generate_compendium(Number - 1).

json_to_file(Path, Json, Prettify) ->
	case Prettify of
		false ->
			file:write_file(Path, Json);
		true ->
			Pretty = jsx:prettify(Json),
			file:write_file(Path, Pretty)
	end.	