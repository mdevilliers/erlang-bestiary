-module (reliable_delivery_api_state_handler).

-export([init/3,content_types_provided/2,get_json/2]).

-include ("reliable_delivery.hrl").

init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.

content_types_provided(Req, State) ->
	{[{{<<"application">>, <<"json">>, []}, get_json}], Req, State}.

get_json(Req, State) ->
	Data = message_store:select_all(),
	DataNewFormat = to_json_format(Data, []),
	DataAsJson = jsx:encode([{<<"currentItems">>, DataNewFormat }]),
	{DataAsJson, Req, State}.


%helpers
to_json_format([], Acc) ->
	Acc;
to_json_format([H|T], Acc) ->
	%H#monitorvalue{identifier = Identifier, value = Value}
	to_json_format(T, [ {<<"identifier">> , H#monitorvalue.identifier , <<"value">>, H#monitorvalue.value| Acc]).