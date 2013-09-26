-module(reliable_delivery_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
	message_store:init(),
    reliable_delivery_sup:start_link().

stop(_State) ->
    ok.
