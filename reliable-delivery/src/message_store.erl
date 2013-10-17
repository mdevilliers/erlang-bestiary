-module (message_store).
-export ([init/0, insert/4, delete/1,lookup/1, select_all/0]).

-include ("reliable_delivery.hrl").

-define (TABLE_ID, ?MODULE).

init() ->
	ets:new(?TABLE_ID,[public, named_table, {keypos, #monitorvalue.identifier}]).

insert(Identifier,Pid, Value, Timeout) ->
	ets:insert(?TABLE_ID, #monitorvalue{ identifier = Identifier, pid = Pid, value =Value, timeout = Timeout, created = calendar:local_time()}).

lookup(Identifier) ->
	case ets:lookup(?TABLE_ID,Identifier) of
		[{monitorvalue, Identifier, Pid, Value, Timeout, Created}] -> {ok, Pid, Value, Timeout, Created};
		[] -> {error, not_found}
	end.

select_all() ->
	ets:match(?TABLE_ID, '$1').

delete(Identifier) ->
	ets:delete(?TABLE_ID,Identifier).