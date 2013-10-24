-module (reliable_delivery_api_state_handler).

-export([init/3,content_types_provided/2,get_json/2]).

-include ("reliable_delivery.hrl").

init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.

content_types_provided(Req, State) ->
	{[{{<<"application">>, <<"json">>, []}, get_json}], Req, State}.

get_json(Req, State) ->
	Data = message_store:select_all(),
	DataNewFormat = iterate_current_items(Data, []),
	%lager:info("~p~n", [DataNewFormat]),
	DataAsJson = jsx:encode([{<<"currentItems">>, DataNewFormat }]),
	{DataAsJson, Req, State}.

%helpers
iterate_current_items([], Acc) ->
	Acc;
iterate_current_items([H|T], Acc) ->
	iterate_current_items(T, [ current_item_to_json_format(H) | Acc]).

current_item_to_json_format([H|_]) ->
	[	{<<"identifier">> , H#monitorvalue.identifier }, 
		{<<"value">>, H#monitorvalue.value }, 
		{<<"created">>, simple_date_to_binary_string(H#monitorvalue.created) },
		{<<"timeout">>, H#monitorvalue.timeout }
	].

simple_date_to_binary_string(Date) ->
	list_to_binary(dh_date:format("D, M Y h:i:s",Date)).