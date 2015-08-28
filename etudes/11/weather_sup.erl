-module (weather_sup).

-behaviour(supervisor).


-export([start_link/0]).

-export([init/1]).	

start_link() ->
 	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->

	WeatherService =  {weather, {weather, start_link, []},
                      permanent, 2000, worker, [weather]},

    {ok, { {one_for_one, 1, 5}, [WeatherService]} }.