-module (reliable_delivery_api_state_handler).

-export([init/3,content_types_provided/2,get_json/2]).

init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.

content_types_provided(Req, State) ->
	{[{{<<"application">>, <<"json">>, []}, get_json}], Req, State}.

get_json(Req, State) ->
	Metrics = folsom_metrics:get_metrics(),
	MetricValues = iterate_metrics(Metrics,[]),
	DataAsJson = jsx:encode([{<<"counters">>, MetricValues }]),
	{DataAsJson, Req, State}.

%helpers
iterate_metrics([], Acc) ->
	Acc;
iterate_metrics([H|T], Acc) -> 
	iterate_metrics(T, [ metric_to_json_format(H) | Acc]).

metric_to_json_format(MetricName) ->
	[{ <<"key">>, atom_to_binary(MetricName,utf8) },{ <<"value">>, folsom_metrics:get_metric_value(MetricName)}].
