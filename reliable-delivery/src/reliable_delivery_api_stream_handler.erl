-module (reliable_delivery_api_stream_handler).
-export([init/4, stream/3, info/3, terminate/2]).

init(_Transport, Req, _Opts, _Active) ->
    {ok, Req, []}.

stream(<<MonitorRequestAsJson/binary>>, Req, State) ->
	lager:info("~p~n", [MonitorRequestAsJson]),
	Request = jsx:decode(MonitorRequestAsJson),
	lager:info("~p~n", [Request]),
	Reply  = find_command(Request),
	{reply, Reply, Req, State};
stream(Data, Req, State) ->
	lager:info("stream received ~s~n", [Data]),
	{ok, Req, State}.

info(_Info, Req, State) ->
    {ok, Req, State}.

terminate(_Req, _State) ->
    ok.

% need to work on this matching clause...
find_command([{<<"monitor">>,[{<<"duration">>,Duration},{<<"value">>,<<Value/binary>>}]}]) ->
	{ok, Identifier} = reliable_delivery:monitor(Duration,Value),
	Identifier.
