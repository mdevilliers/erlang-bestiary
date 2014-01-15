-module (defines_a_behaviour).

-export([behaviour_info/1]).
 
behaviour_info(callbacks) ->
    [ {init,1}, {handle, 1}];
behaviour_info(_Other) ->
    undefined.