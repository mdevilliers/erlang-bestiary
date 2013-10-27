-module (reliable_delivery_uuid).

-behaviour(gen_server).

-export([start_link/0]).
-export([gen/0, gen_secure/0, string/2, binary/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Public API
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [0],[]).

fresh() ->
    Serial = gen_server:call( ?MODULE, serial, infinity),
    {Serial, node(), make_ref()}.

advance_blocks({B1, B2, B3, B4}, I) ->
    B5 = erlang:phash2({B1, I}, 4294967296),
    {{(B2 bxor B5), (B3 bxor B5), (B4 bxor B5), B5}, I+1}.

gen() ->
    case get(guid) of
        undefined -> <<B1:32, B2:32, B3:32, B4:32>> = Res =
                         erlang:md5(term_to_binary(fresh())),
                     put(guid, {{B1, B2, B3, B4}, 0}),
                     Res;
        {BS, I}   -> {{B1, B2, B3, B4}, _} = S = advance_blocks(BS, I),
                     put(guid, S),
                     <<B1:32, B2:32, B3:32, B4:32>>
    end.

gen_secure() ->
    G = case get(guid_secure) of
            undefined -> {fresh(), 0};
            {S, I}    -> {S, I+1}
        end,
    put(guid_secure, G),
    erlang:md5(term_to_binary(G)).

string(G, Prefix) ->
    Prefix ++ "-" ++ base64url(G).

binary(G, Prefix) ->
    list_to_binary(string(G, Prefix)).

base64url(In) ->
    lists:reverse(lists:foldl(fun ($\+, Acc) -> [$\- | Acc];
                                  ($\/, Acc) -> [$\_ | Acc];
                                  ($\=, Acc) -> Acc;
                                  (Chr, Acc) -> [Chr | Acc]
                              end, [], base64:encode_to_string(In))).

% others
init([Serial]) ->
    {ok, Serial}.

handle_call(serial, _From, State) ->
  {reply, State, State};
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
