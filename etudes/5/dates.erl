-module (dates).
-export ([date_parts/1]).


date_parts(DateStr) ->
	re:split(DateStr, "[-]", [{return,list}]).


