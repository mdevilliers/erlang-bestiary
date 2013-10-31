-module (reliable_delivery_api_state_handler).

-export([init/3,content_types_provided/2,get_json/2]).

init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.

content_types_provided(Req, State) ->
	{[{{<<"application">>, <<"json">>, []}, get_json}], Req, State}.

get_json(Req, State) ->
	MetricValues = reliable_delivery_monitor_stats:all_metrics_with_values(),
	DataAsJson = jsx:encode([{<<"counters">>, MetricValues }]),
	{DataAsJson, Req, State}.
