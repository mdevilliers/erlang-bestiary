-module(reliable_delivery_app).

-behaviour(application).

-export([start/2, stop/1]).

-define(C_ACCEPTORS,  100).

start(_StartType, _StartArgs) ->

	% start cowboy
	Routes    = routes(),
    Dispatch  = cowboy_router:compile(Routes),
    Port      = port(),
    TransOpts = [{port, Port}],
    ProtoOpts = [{env, [{dispatch, Dispatch}]}],
    {ok, _}   = cowboy:start_http(http, ?C_ACCEPTORS, TransOpts, ProtoOpts),

	message_store:init(),
    reliable_delivery_sup:start_link().
stop(_State) ->
    ok.

% helpers
routes() ->
    [
     {'_', [
            {"/", reliable_delivery_index_handler, []}
           ]}
    ].

port() ->
    case os:getenv("PORT") of
        false ->
            {ok, Port} = application:get_env(http_port),
            Port;
        Other ->
            Other
    end.
