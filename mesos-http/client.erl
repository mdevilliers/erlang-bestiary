-module (client).

-behaviour(gen_server).

-export ([start_link/0, register/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record (state, { }).

start_link() ->
	application:ensure_started(inets),
	gen_server:start({local, ?MODULE}, ?MODULE, [], []).

register() ->
	Register = <<"{ \"type\" : \"SUBSCRIBE\",  \"subscribe\" : { \"framework_info\" : { \"user\" : \"two\", \"name\" : \"erlang\"}} }">>,

	gen_server:call(?MODULE, {register, Register}).

init([]) ->
  	{ok, #state{}}.

handle_call({register, Register}, _From, State) ->

	Method = post,
	URL = "http://mesos-dev:5050/api/v1/scheduler",
	Header = [],
	Type = "application/json",
	Body = Register,
	HTTPOptions = [],
	Options = [{sync, false}, {stream, self}],
	{ok, R}= httpc:request(Method, {URL, Header, Type, Body}, HTTPOptions, Options),
	{reply, {ok,R}, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(Info, State) ->
  io:format("Info from schedular ~p~n", [Info]),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

