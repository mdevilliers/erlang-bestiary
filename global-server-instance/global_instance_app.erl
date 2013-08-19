-module(global_instance_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, join_node/1, list_nodes/0]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

join_node(Node) ->
	net_kernel:connect_node(Node).

list_nodes()->
	io:format("Nodes : ~p~n", [nodes()]),
	ok.

start(_StartType, _StartArgs) ->
	io:format("application starting~n", []),
    global_instance_sup:start_link().

stop(_State) ->
    ok.
