-module (cards).

-export ([make_deck/0, show_deck/1]).


make_deck() ->
	[{Value , Suit} ||  Value <- [ "A", 2, 3, 4, 5, 6, 7, 8, 9, 10, "J", "Q", "K"], Suit <- ["Club", "Spade", "Heart", "Diamond"]].
	
show_deck(Deck) ->
	lists:foreach(fun(Item) -> io:format("~p~n", [Item]) end, Deck).