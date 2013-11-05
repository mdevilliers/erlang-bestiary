-module (reliable_delivery_api_stream_handler).
-export([init/4, stream/3, info/3, terminate/2]).


init(_Transport, Req, _Opts, _Active) ->
    {ok, Req, []}.

stream(<<RequestAsJson/binary>>, Req, State) ->
	Request = jsx:decode(RequestAsJson),
	Reply =  execute_command(Request),
	ReplyAsJson  = jsx:encode(Reply),
	{reply, ReplyAsJson, Req, State};
stream(Data, Req, State) ->
	lager:info("stream received ~s~n", [Data]),
	{ok, Req, State}.


info({expired, Identifier, Value}, Req, State) ->
	lager:info("Info : Expired : ~p~n",[Identifier]),
	ReplyAsJson = jsx:encode([{<<"expired">> , [{<<"identifier">>, Identifier}, {<<"value">>, Value}] }]),
	{reply, ReplyAsJson, Req, State};
info(Info, Req, State) ->
	lager:info("Info : ~p~n",[Info]),
    {ok, Req, State}.

terminate(_Req, _State) ->
    ok.

execute_command([{<<"register">>,Properties}]) ->
	{_,Application} = lists:keyfind(<<"application">>,1,Properties),
	store_application_mapping(Application),
	[{ <<"successful">>, <<"ok">>}];
execute_command([{<<"monitor">>,Properties}]) ->
	{_,Duration} = lists:keyfind(<<"duration">>,1,Properties),
	{_,Value} = lists:keyfind(<<"value">>,1,Properties),
	{_,Application} = lists:keyfind(<<"application">>,1,Properties),
	{ok, Identifier} = reliable_delivery:monitor(Duration,Application,Value),
	[{ <<"reference">>, Identifier}].


% idemnepotent - gproc throws an exception if you add the same property twice?
store_application_mapping(Application) ->
	try  gproc:add_local_property({application}, Application)
	catch
		_:_ -> ok
	end,
	ok.
