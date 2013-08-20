
-module (process_1).
-export ([start/2,loop/0]).

% starts 2 processes, and sends a message M times forwards and backwards between them. After the messages have been sent the processes should terminate gracefully.

start(M , Messagse) ->
	
	P1 = spawn(process_1, loop , []),
	P2 = spawn(process_1, loop , []),
	P1 ! {P2, P1, Message, M}.

loop() ->
	receive
		stop ->
			io:format("Finished and shutting down ~p .~n", [self()]);
		{_, To, _, 0} ->
			To ! stop,
			io:format("Sending Stop to ~p and shutting down ~p .~n", [To, self()]);
		{From, To, Message, Count} ->
			io:format("Recieved : ~s From : ~p  Sending To : ~p  Count : ~p. ~n", [Message, From, To, Count]),
			To ! { To, From, Message, Count - 1},
			loop()
	end.