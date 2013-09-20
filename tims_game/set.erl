-module (set).
-export ([new_board/0, new_board/1, new_board/2]).

% api
new_board(MinimumSetsToInclude) ->
	new_board(MinimumSetsToInclude, 9999).

new_board(MinimumSetsToInclude, MaximunSetsToInclude) ->
	{Length, Cards, Combinations} = new_board(),
	case (Length =< MaximunSetsToInclude) and (Length >= MinimumSetsToInclude) of
		false ->
			new_board(MinimumSetsToInclude, MaximunSetsToInclude)  ;
		true ->
			{Length, Cards, Combinations} 
	end.

new_board() ->
	Deck = make_deck(),
	ShuffledDeck = shuffle(Deck),
	Board = top(ShuffledDeck,16),
	TotalSets = count_sets(Board),
	{length(TotalSets), Board, TotalSets}.

% helpers
count_sets(Deck) ->
	AllCombinations = combinations(3, Deck),
	count_sets(AllCombinations,[]).

count_sets([], Acc) ->
	Acc;
count_sets([H|T], Acc) ->
	case check_combo(H) of
		true -> count_sets(T, [H|Acc]);
		false -> count_sets(T, Acc)
	end.

check_combo([{C1, S1, F1, N1}, {C2, S2, F2, N2}, {C3, S3, F3, N3}]) ->
	check_attribute(C1, C2, C3) and check_attribute(S1, S2, S3) and check_attribute(F1, F2, F3) and check_attribute(N1, N2, N3).

check_attribute(A,A,A) ->
	true;
check_attribute(A,A,_) ->
	false;
check_attribute(A,_,A) ->
	false;
check_attribute(_,A,A) ->
	false;
check_attribute(_,_,_) ->
	true.

top(Deck, N) ->
	lists:sublist(Deck,N).

make_deck() ->
	[{Colour , Shape, Fill, Number} || 
		Colour <- [red, green, blue ], 
		Shape  <- [wotsit, sausage, diamond], 
		Fill   <- [empty, hatched, solid], 
		Number <- [1,2,3]].

shuffle(Deck) -> shuffle(Deck, []).
shuffle([], Acc) -> Acc;
shuffle(Deck, Acc) ->
  {Leading, [H | T]} = lists:split(random:uniform(length(Deck)) - 1, Deck),
  shuffle(Leading ++ T, [H | Acc]).

combinations(1, L) -> [[X] || X <-L];
combinations(K, L) when K == length(L) -> [L];
combinations(K, [H|T]) ->
    [[H | Subcombos] || Subcombos <- combinations(K-1, T)]
    ++(combinations(K, T)).
