-module (reliable_delivery_monitor_store).

-behaviour(gen_server).

-export ([start_link/0, delete/1,lookup/1,insert/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include ("reliable_delivery.hrl").

-define (TABLE_ID, in_memory_monitor_store).

%% Public API
start_link() ->
  gen_server:start({local, ?MODULE}, ?MODULE, [], []).

delete(Identifier) ->
	gen_server:call(?MODULE, {delete, Identifier}).
lookup(Identifier) ->
	gen_server:call(?MODULE, {lookup, Identifier}).
insert(Identifier, Pid) ->
	gen_server:call(?MODULE, {insert, #monitorvalue{ 	identifier = Identifier, 
														pid = Pid
													}}).

init([]) ->
  	ets:new(?TABLE_ID,[protected, named_table, {keypos, #monitorvalue.identifier}]),
  	{ok, []}.

handle_call({insert, Value}, _From, State) ->
	Reply = ets:insert(?TABLE_ID,Value),
  	{reply,Reply,State};

handle_call({lookup, Identifier}, _From, State) ->
	case ets:lookup(?TABLE_ID,Identifier) of
		[{monitorvalue, Identifier, Pid}] ->
			Reply = {ok, Identifier, Pid};
		[] -> 
			Reply = {error, not_found}
	end,
  	{reply,Reply,State};

handle_call({delete, Identifier}, _From, State) ->
	Reply = ets:delete(?TABLE_ID,Identifier),
  	{reply,Reply,State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.