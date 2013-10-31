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
    ProtoOpts = [{compress, true},{env, [{dispatch, Dispatch}]}],
    {ok, _}   = cowboy:start_http(http, ?C_ACCEPTORS, TransOpts, ProtoOpts),

    reliable_delivery_sup:start_link().


stop(_State) ->
    ok.

% helpers
routes() ->
    [
     {'_', [
            {"/api/bullet", bullet_handler , [{handler, reliable_delivery_api_stream_handler}]},
            {"/api/statistics", reliable_delivery_api_state_handler, []},
            {"/", cowboy_static, 
            	[
                    {directory, {priv_dir, reliable_delivery, []}},
                    {file, "index.html"},
                    {mimetypes, {fun mimetypes:path_to_mimes/2, default}}
                ]
            },
            {"/assets/[...]", cowboy_static, 
            	[
                	{directory, {priv_dir, reliable_delivery, [<<"assets">>]}},
                    {mimetypes, {fun mimetypes:path_to_mimes/2, default}}
                ]
            }
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
