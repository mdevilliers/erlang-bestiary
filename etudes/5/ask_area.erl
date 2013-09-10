-module (ask_area).
-export ([area/0]).

area() ->
	ShapeInput = io:get_line("R)ectangle, T)riangle or E)llipse >"),
	Shape = char_to_shape( hd(ShapeInput)),

	case Shape of
		unknown ->
			io:format("Unknown shape " ++ ShapeInput);
		_ -> 
			case Shape of
				rectangle  ->
					{Dim1, Dim2} = get_dimensions("width", "height");
				triangle  ->
					{Dim1, Dim2} = get_dimensions("base", "height");
				ellipse  ->
					{Dim1, Dim2} = get_dimensions("major axis", "minor axis")
			end,

			case calculate(Shape, Dim1, Dim2) of
				{ok,Answer}  ->
					io:format("~p~n",[Answer]);
				{error,Reason} ->
					io:format(Reason ++ "\n")
			end
	end.

char_to_shape(Char) ->
	case Char of
		$R -> rectangle;
		$r->  rectangle;
		$T -> triangle;
		$t -> triangle;
		$E -> ellipse;
		$e -> ellipse;
		_ ->
			unknown
	end.

get_number(Prompt) ->
	Result = io:get_line("Enter " ++ Prompt ++ " > "),
	{Parsed, _ } = string:to_float(Result),

	case Parsed of
		error ->
			{Parsed0, _ } = string:to_integer(Result);
		_ -> Parsed0 = Parsed
	end,
	Parsed0.

get_dimensions(Prompt1, Prompt2) ->
	A = get_number(Prompt1),
	B = get_number(Prompt2),
	{A,B}.

calculate(_, error, _ ) ->
	{error, "Error in first number"};
calculate(_, _, error) ->
	{error, "Error in second number"};
calculate(_, Dim1, Dim2 ) when Dim1 < 0; Dim2 < 0 ->
	{error, "Both numbers need to be greater than 0."};
calculate(Shape, Dim1, Dim2) ->
	geom:area({Shape,Dim1,Dim2}).