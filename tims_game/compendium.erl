-module (compendium).

-export ([generate_games/0, json_to_file/3]).
-export ([init/0, find_all_games_by_sets/1, find_random_game_by_set/1]).
-export([do_generate_games/1]).
	
-define (TABLE_ID, ?MODULE).

-record (board, {hash, sets, json}).

generate_games() ->
	spawn(?MODULE, do_generate_games, [15]) ,
	spawn(?MODULE, do_generate_games, [15]) ,
	spawn(?MODULE, do_generate_games, [15]) ,
	spawn(?MODULE, do_generate_games, [15]) ,
	spawn(?MODULE, do_generate_games, [15]) ,
	ok.

init() ->
	ets:new(?TABLE_ID,[duplicate_bag, public, named_table, {keypos, #board.sets}]).

find_random_game_by_set(Sets) ->
	{ok, Games} = find_all_games_by_sets(Sets),
	lists:sublist(Games, random:uniform(length(Games) -1), 1).

find_all_games_by_sets(Sets) ->
	case ets:lookup(?TABLE_ID, Sets) of
		[] -> {error, not_found};
		Games -> {ok, Games}
	end.

json_to_file(Path, Json, Prettify) ->
	case Prettify of
		false ->
			file:write_file(Path, Json);
		true ->
			Pretty = jsx:prettify(Json),
			file:write_file(Path, Pretty)
	end.

% helpers
insert_game(Hash, Sets,Json) ->
	ets:insert(?TABLE_ID, #board{ hash = Hash, sets = Sets, json = Json}).

do_generate_games(Number) when Number < 1 ->
	ok;
do_generate_games(Number) ->
	random:seed(now()),
	Board = set:new_board(Number, Number),
	Json = set:game_as_json(Board),
	%json_to_file("game_" ++ integer_to_list(Number) ++ "_" ++ integer_to_list(Repeat) ++ ".json", Json, false),
	% TODO : check for duplicate hash
	Hash = erlang:phash2(Json), 
	io:format("~p : ~p~n", [Number, Hash]),
	insert_game(Hash, Number, Json),
	do_generate_games(Number - 1).
