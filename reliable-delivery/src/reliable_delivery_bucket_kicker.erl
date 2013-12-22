-module (reliable_delivery_bucket_kicker).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Public API
start_link() ->
  gen_server:start_link(?MODULE, [],[]).

% others
init(_) ->
	reliable_delivery_event:subscribe(bucket_info),
  {ok, []}.

handle_call(_Request, _From, State) ->
  	{reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({ _ ,{bucket_info},{new, NewBucket}}, State) ->
  reliable_delivery_bucket_sup:start_bucket_worker(NewBucket),
  {noreply, State};
handle_info(_, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_, State, _) ->
  {ok, State}.
