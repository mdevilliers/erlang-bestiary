-module (cards).

-export ([make_deck/0, show_deck/1, shuffle/1, deal/2]).

make_deck() ->
	[{Value , Suit} ||  Value <- [ "A", 2, 3, 4, 5, 6, 7, 8, 9, 10, "J", "Q", "K"], Suit <- ["Club", "Spade", "Heart", "Diamond"]].
	
show_deck(Deck) ->
	lists:foreach(fun(Item) -> io:format("~p~n", [Item]) end, Deck).

shuffle(Deck) ->
	[X||{_,X} <- lists:sort([ {random:uniform(), N} || N <- Deck])].

deal(Deck, Players) ->
	MaxCards = length(Deck) div Players,
	Result = do_deal(Deck, MaxCards),

    case length(Result) of
        Players -> Result;
        _ -> 
            lists:nthtail(1, Result)
    end.

do_deal(List, Max) ->
    element(1, lists:foldl(fun
        (E, {[Buff|Acc], C}) when C < Max ->
            {[[E|Buff]|Acc], C+1};
        (E, {[Buff|Acc], _}) ->
            {[[E],Buff|Acc], 1};
        (E, {[], _}) ->
            {[[E]], 1}
    end, {[], 0}, List)).