-module (reliable_delivery_event).

-behaviour (gen_event).
-export ([code_change/3,handle_call/2,handle_event/2,handle_info/2,init/1,terminate/2]).
-export ([start_link/0, add_handler/1, remove_handler/1, emit_tick/1]).

start_link() ->
	gen_event:start_link(?MODULE).

add_handler(Pid) ->
	gen_event:add_handler(?MODULE, Pid, []).

remove_handler(Pid) ->
	gen_event:delete_handler(?MODULE, Pid, []).

emit_tick(tick) ->
	gen_event:notify(?MODULE, tick).

init(InitArgs) -> 
	{ok,InitArgs}.
handle_info(_, State) ->
	{ok,State}.
handle_call(_, State) ->
	{ok,ok,State}.
handle_event(_, State) ->
	{ok,State}.
code_change(_, State, _) ->
  	{ok, State}.
terminate(_, _State) ->
	ok.