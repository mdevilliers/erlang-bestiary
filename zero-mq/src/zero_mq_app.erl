-module(zero_mq_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    zero_mq_sup:start_link().

stop(_State) ->
    ok.
