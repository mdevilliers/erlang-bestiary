-module (reliable_delivery_uuid).

-behaviour(gen_server).

-export([start_link/0]).
-export([generate/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Public API
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [],[]).

generate() ->
   uuid:uuid_to_string(uuid:get_v4(weak), binary_standard).

% others
init([]) ->
    {ok, []}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_, State, _) ->
  {ok, State}.
