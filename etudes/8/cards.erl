-module (cards).

-export ([make_deck/0, show_deck/1, shuffle/1]).


make_deck() ->
	[{Value , Suit} ||  Value <- [ "A", 2, 3, 4, 5, 6, 7, 8, 9, 10, "J", "Q", "K"], Suit <- ["Club", "Spade", "Heart", "Diamond"]].
	%[{Value , Suit} ||  Value <- [ "A", 2 , 3 , 4], Suit <- ["Club", "Spade"]].
	
show_deck(Deck) ->
	lists:foreach(fun(Item) -> io:format("~p~n", [Item]) end, Deck).

shuffle(Deck) -> shuffle(Deck, []).
shuffle([], Acc) -> Acc;
shuffle(Deck, Acc) ->
  {Leading, [H | T]} = lists:split(random:uniform(length(Deck)) - 1, Deck),
  shuffle(Leading ++ T, [H | Acc]).