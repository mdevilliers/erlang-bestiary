-module(global_instance_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, join_node/1, list_nodes/0]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

join_node(Node) ->
	case net_kernel:connect(Node) of
		true ->
	    		ok;
		false ->
		    	timer:sleep(1000),
		    	io:format("Retrying connect to ~p~n", [Node]),
		    	join_node(Node)
    end.

list_nodes()->
	io:format("Nodes : ~p~n", [nodes()]),
	ok.

start(_StartType, _StartArgs) ->
	io:format("application starting ~p~n", [self()]),
    global_instance_sup:start_link().

stop(_State) ->
    ok.
