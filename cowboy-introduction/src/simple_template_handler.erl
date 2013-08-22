-module(simple_template_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, _Opts) ->
    {ok, Req, undefined_state}.
 
handle(Req, State) ->
    {ok, Body} = mylittlepony_dtl:render([{pony_name, "Dazzleglow"}]),
    Headers = [{<<"Content-Type">>, <<"text/html">>}],
    {ok, Req2} = cowboy_req:reply(200, Headers, Body, Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.