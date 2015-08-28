-module (weather).

-export ([start_link/0, report/1, recent/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("xmerl/include/xmerl.hrl").

%% Public API
start_link() ->
  gen_server:start({local, ?MODULE}, ?MODULE, [], []).

report( WeatherStationIdentifier) ->
	gen_server:call(weather, {weather, WeatherStationIdentifier}).

recent() ->
	gen_server:call(weather, {recent}).


%% Impl %%
init([]) ->
	application:ensure_started(inets),
  	{ok, []}.

handle_call({recent}, _From, State) ->
	{reply, State, State};
handle_call({weather, WeatherStationIdentifier}, _From, State) ->

	Url = "http://w1.weather.gov/xml/current_obs/" ++ WeatherStationIdentifier ++ ".xml",
	{Result, Info} = httpc:request(Url),

	case Result of
		error -> {reply, {error, Result, State }};
		ok -> 
			{{_Protocol, Code, _CodeStr}, _Attrs, WebData} = Info,
      		case Code of
        		404 ->
          			{reply, {error, 404}, State};
        		200 ->
          			Weather = analyze_info(WebData),
          			{reply, {ok,Weather}, [ WeatherStationIdentifier | State]}
      		end	
	end;
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%% Take raw XML data and return a set of {key, value} tuples
analyze_info(WebData) ->
  %% list of fields that you want to extract
  ToFind = [location, observation_time_rfc822, weather, temperature_string],

  %% get just the parsed data from the XML parse result
  Parsed = element(1, xmerl_scan:string(WebData)),

  %% This is the list of all children under <current_observation>
  Children = Parsed#xmlElement.content,

  %% Find only XML elements and extract their names and their text content.
  %% You need the guard so that you don't process the newlines in the
  %% data (they are XML text descendants of the root element).
  ElementList = [{El#xmlElement.name, extract_text(El#xmlElement.content)}
    || El <- Children, element(1, El) == xmlElement],

  %% ElementList is now a keymap; get the data you want from it.
  lists:map(fun(Item) -> lists:keyfind(Item, 1, ElementList) end, ToFind).


%% Given the parsed content of an XML element, return its first node value
%% (if it's a text node); otherwise return the empty string.

extract_text(Content) ->
  Item = hd(Content),
  case element(1, Item) of
    xmlText -> Item#xmlText.value;
    _ -> ""
  end.
