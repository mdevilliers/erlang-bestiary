
-module (zero_mq_publisher).
-compile(export_all).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Public API

listen() ->
  
    {ok, Context} = erlzmq:context(),

    %%  Subscriber tells us when it's ready here
    {ok, Sync} = erlzmq:socket(Context, pull),
    ok = erlzmq:bind(Sync, "tcp://*:5564"),

    %%  We send updates via this socket
    {ok, Publisher} = erlzmq:socket(Context, pub),
    ok = erlzmq:bind(Publisher, "tcp://*:5565"),

    %%  Wait for synchronization request
    {ok, _} = erlzmq:recv(Sync),

    %%  Now broadcast exactly 10 updates with pause
    lists:foreach(
      fun(Num) ->
              Msg = list_to_binary(io_lib:format("Update ~b", [Num])),
              ok = erlzmq:send(Publisher, Msg),
              timer:sleep(1000)
      end, lists:seq(1, 10)),
    erlzmq:send(Publisher, <<"END">>),

    erlzmq:close(Sync),
    erlzmq:close(Publisher),
    erlzmq:term(Context).



start() ->
  gen_server:start({local, ?MODULE}, ?MODULE, [], []).

stop(Module) ->
  gen_server:call(Module, stop).

stop() ->
  stop(?MODULE).

state(Module) ->
  gen_server:call(Module, state).

state() ->
  state(?MODULE).

%% Server implementation, a.k.a.: callbacks

init([]) ->

  {ok, []}.

handle_call(stop, _From, State) ->
  say("stopping by ~p, state was ~p.", [_From, State]),
  {stop, normal, stopped, State};

handle_call(state, _From, State) ->
  say("~p is asking for the state.", [_From]),
  {reply, State, State};

handle_call(_Request, _From, State) ->
  say("call ~p, ~p, ~p.", [_Request, _From, State]),
  {reply, ok, State}.


handle_cast(_Msg, State) ->
  say("cast ~p, ~p.", [_Msg, State]),
  {noreply, State}.


handle_info(_Info, State) ->
  say("info ~p, ~p.", [_Info, State]),
  {noreply, State}.


terminate(_Reason, _State) ->
  say("terminate ~p, ~p", [_Reason, _State]),
  ok.


code_change(_OldVsn, State, _Extra) ->
  say("code_change ~p, ~p, ~p", [_OldVsn, State, _Extra]),
  {ok, State}.

%% Some helper methods.

say(Format) ->
  say(Format, []).
say(Format, Data) ->
  io:format("~p:~p: ~s~n", [?MODULE, self(), io_lib:format(Format, Data)]).
