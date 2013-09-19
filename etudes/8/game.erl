-module (game).
-export ([new_game/0, play/1]).

% model { gamestate , player one card, player two card, player one hand, player two hand, cards in the middle (if any)}
% gamestate - game_started, game_in_progress, game_over, game_over_draw

new_game() ->
	random:seed(now()),
	Deck = cards:make_deck(),
	Shuffled = cards:shuffle(Deck),
	{PlayerOne , PlayerTwo} = split_deck(Shuffled),
	{game_started, nocard, nocard, PlayerOne, PlayerTwo, []}.

play(Game) ->
	Game0 = next(Game),
	Game1 = decide(Game0),
	case Game1 of
		{game_over_draw, _, _ , _, _, _} ->
			io:format("draw - no-one wins... ~n");
		{game_over, _, _ , _, [], _} ->
			io:format("player one wins ~n");
		{game_over, _, _ , [], _, _} ->
			io:format("player two wins ~n");
		_ -> 
			io:format("~p~n" , [Game1]),
			%io:get_line(" >"),
			play(Game1)
	end.

decide({game_in_progress, Card1, Card2 , PlayerOne, PlayerTwo, Bank}) ->
	case is_higher_card(Card1, Card2)   of
		draw ->
			io:format("D ~p == ~p~n", [Card1, Card2]),
			{game_in_progress, nocard, nocard , PlayerOne, PlayerTwo, collect_cards(Card1, Card2, Bank)};
		false -> 
			io:format("2 ~p < ~p~n", [Card1, Card2]),
			{game_in_progress, nocard, nocard , PlayerOne, collect_cards(Card1, Card2, Bank, PlayerTwo), []};
		true ->
			io:format("1 ~p > ~p~n", [Card1, Card2]),
			{game_in_progress, nocard, nocard , collect_cards(Card1, Card2, Bank, PlayerOne), PlayerTwo, []}
	end.

next({ _, _, _, [], [], _ }) ->
	{game_over_draw , nocard, nocard , [], [], []} ;
next({ _, _, _, PlayerOne, [], _ }) ->
	{game_over , nocard, nocard , PlayerOne, [], []} ;
next({ _, _, _, [], PlayerTwo, _ }) ->
	{game_over , nocard, nocard , [], PlayerTwo, []} ;
next({ _, _, _, [H1| T1], [H2 | T2], Bank }) ->
	{game_in_progress, H1, H2, T1, T2, Bank}.

%helpers
is_higher_card({Card1,_}, {Card2,_}) ->
	Value1 = score_card_value( Card1 ),
	Value2 = score_card_value( Card2 ),
	if
		Value1 =:= Value2 ->
			draw;
		Value2 > Value1 ->
			false;
		true ->
			true
	end.

score_card_value(Value) ->
	case Value of
		"J" -> 11;
		"Q" -> 12;
		"K" -> 13;
		"A" -> 14;
		_ -> Value
	end.

split_deck(Deck) ->
	split_deck(Deck, [], []).

split_deck([], PlayerOne, PlayerTwo) ->
	{ PlayerOne, PlayerTwo};
split_deck([H1 , H2 | T], PlayerOne, PlayerTwo) ->
	split_deck(T, [H1|PlayerOne], [H2|PlayerTwo]).

collect_cards(Card1, Card2, Bank) ->
	lists:flatten([Card1 | [Card2 | Bank ]]).
collect_cards(Card1, Card2, Bank, Hand) ->
	lists:flatten(Hand, lists:flatten([Card1 | [Card2 | Bank ]])).