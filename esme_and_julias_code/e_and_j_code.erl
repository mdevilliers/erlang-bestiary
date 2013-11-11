-module (e_and_j_code).
-export ([encode/1]).

%
% A B C D E F G H I J K L M N O P Q R S T U V W X Y Z 
% Z Y X W V U T S R Q P O N M L K J I H G F E D C B A
%
% http://en.wikipedia.org/wiki/Atbash
%

encode([H|T]) ->
	do_encode(H,T,[]).

do_encode(Current, [], Acc) ->
 	lists:flatten(lists:reverse([e(Current)|Acc]));
do_encode(Current, [H|T], Acc) ->
	do_encode(H, T, [e(Current)|Acc]).

e(Char) when (Char >= $A andalso Char =< $Z)
	 -> $Z + $A - Char;
e(Char) when (Char >= $a andalso Char =< $z)
	 -> $z + $a - Char;
e(Char) -> Char.
