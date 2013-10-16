-module (message_store).

-export ([init/0, insert/3, delete/1,lookup/1, select_all/0]).

-define (TABLE_ID, ?MODULE).

-record (monitorvalue, {identifier, pid, value }).

init() ->
	ets:new(?TABLE_ID,[public, named_table, {keypos, #monitorvalue.identifier}]).

insert(Identifier,Pid, Value) ->
	ets:insert(?TABLE_ID, #monitorvalue{ identifier = Identifier, pid = Pid, value =Value}).

lookup(Identifier) ->
	case ets:lookup(?TABLE_ID,Identifier) of
		[{monitorvalue,Identifier, Pid, Value}] -> {ok, Pid, Value};
		[] -> {error, not_found}
	end.

select_all() ->
	ets:match(?TABLE_ID, '$1').

delete(Identifier) ->
	ets:delete(?TABLE_ID,Identifier).