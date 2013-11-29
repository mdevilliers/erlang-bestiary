-module (reliable_delivery_ticker).

-behaviour(gen_server).

-export ([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(INTERVAL, 1000). % One second

%% Public API
start_link() ->
  gen_server:start({local, ?MODULE}, ?MODULE, [0], []).

init([InitialValue]) ->
	erlang:send_after(?INTERVAL, self(), trigger),
  	{ok, InitialValue}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(trigger, InitialValue) ->
	InitialValue1 = InitialValue + 1,
	io:format("TicK ~p~n", [InitialValue1]),
	erlang:send_after(?INTERVAL, self(), trigger),
	{noreply, InitialValue1};
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.