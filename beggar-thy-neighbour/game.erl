-module (game).

-export ([new_game/1, play/1]).
-export ([player_loop/1, game_loop/2, decision/1]).

-record (game, {players = [], centre = []}).

% impl of beggarthyneigbour card game
% -------------------------------------------------------------
% NOTE : this is the version played in the deVilliers household
% -------------------------------------------------------------

new_game(NoOfPlayers) ->
	Shuffled = cards:shuffle(cards:make_deck()),
	Delt = cards:deal(Shuffled, NoOfPlayers),
	Players = lists:map( fun (H)-> spawn_link(?MODULE, player_loop, [H]) end, Delt),
	#game{players = Players}.

play(Game) ->
	spawn_link(?MODULE, game_loop, [{ok}, Game]).
	
decision({"A",_}) -> {must_play, 4};
decision({"K",_}) -> {must_play, 3};
decision({"Q",_}) -> {must_play, 2};
decision({"J",_}) -> {must_play, 1};
decision({_,_}) -> { ok }.

game_loop( {must_play, 1}, #game{ players = [CurrentPlayer|Rest], centre = Centre}) ->

	CurrentPlayer ! {next_card, self()},

	receive
		{Action, Card} ->

			Decision = decision(Card),
			Pot = [Card | Centre],
			
			case Action of
				play ->
					case Decision of
						{ok} ->
							NewState =  #game{ players = lists:flatten(Rest,[CurrentPlayer]), centre = []},
							debug_game("pick up pot", Decision, NewState),
							CurrentPlayer ! {pick_up, Pot},
							game_loop( {ok}, NewState);
						_ ->
							NewState =  #game{ players = lists:flatten(Rest,[CurrentPlayer]), centre = Pot},
							debug_game("next player", Decision, NewState),
							game_loop( Decision, NewState)
					end;
				no_more_cards ->
					case is_game_over(Rest) of
						true -> io:format("GAME OVER! WINNER : ~p ~n", [Rest]);
						_ -> 
							NewState = #game{ players = Rest, centre = Pot},
							debug_game("Player out", Decision, NewState),
							game_loop( Decision, NewState)
					end
			end
	end;

game_loop( {must_play, N}, #game{ players = [CurrentPlayer|Rest] = Players, centre = Centre}) ->

	CurrentPlayer ! {next_card, self()},

	receive 
			{Action, Card} ->

				Decision = decision(Card),
				Pot = [Card | Centre],
			
				case Action of
				play ->
					case Decision of
						{ok} ->
							NewState = #game{ players = Players, centre = Pot}, 
							debug_game("same player", Decision, NewState),
							game_loop( {must_play, N - 1},  NewState);
						_ ->
							NewState =  #game{ players =  lists:flatten(Rest,[CurrentPlayer]), centre = Pot},
							debug_game("next player", Decision, NewState),
							game_loop( Decision, NewState)
					end;
				no_more_cards ->
					case is_game_over(Rest) of
						true -> io:format("GAME OVER! WINNER : ~p ~n", [Rest]);
						_ -> 
							NewState = #game{ players = Rest, centre = Pot},
							debug_game("Player out", Decision, NewState),
							game_loop( Decision, NewState)
					end
			end
	end;

game_loop( {ok}, #game{ players = [CurrentPlayer|Rest], centre = Centre}) ->

	CurrentPlayer ! {next_card, self()},

	receive 
			{Action, Card} ->

				Decision = decision(Card),
				Pot = [Card | Centre],

				case Action of
				play ->
					NewState =  #game{ players = lists:flatten(Rest,[CurrentPlayer]), centre = Pot},
					debug_game("next player", Decision, NewState),
					game_loop( Decision, NewState);
				no_more_cards ->
					case is_game_over(Rest) of
						true -> io:format("GAME OVER! WINNER : ~p ~n", [Rest]);
						_ -> 
							NewState = #game{ players = Rest, centre = Pot},
							debug_game("Player out", Decision, NewState),
							game_loop( Decision, NewState)
					end
			end
	end.

player_loop(Hand) -> 
	receive	
		{next_card, From} ->
			case Hand of
				[H] -> 
					debug_hand("last card", self(), [H]),
					From ! {no_more_cards, H};
				[H|T] -> 
					debug_hand("next card", self(), H, T),
					From ! {play, H},
					player_loop(T)
			end;
		{pick_up, Cards} ->
				NewHand = lists:flatten (Hand, Cards),
				debug_hand("pickup pot", self(), NewHand),
				player_loop(NewHand)
		end.	

is_game_over(Players) when length(Players) == 1 -> true; 
is_game_over(_)-> false. 

debug_game(Message, Decision, #game{ players = Players, centre = Centre}) ->
	io:format("~s, Decision : ~p,Players : ~p, Centre : ~p ~n", [Message, Decision, Players, length(Centre)]).

debug_hand(Message, Ego, Hand) ->
	io:format("~p : ~s, Hand : ~p ~n", [Ego, Message, length(Hand)]).
debug_hand(Message, Ego, Card, Hand) ->
	io:format("~p : ~s, Card : ~p, Hand : ~p ~n", [Ego, Message, Card, length(Hand)]).
