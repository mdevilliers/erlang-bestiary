-module (reliable_delivery_monitor_stats).

-behaviour(gen_server).

-export ([	start_link/0, 
			increment_total_monitors/0, 
			increment_current_monitors/0,
			decrement_current_monitors/0, 
			increment_unknown_monitors/0, 
			increment_acked_monitors/0,
			increment_expired_monitors/0,
			all_metrics_with_values/0
		]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Public API
start_link() ->
  gen_server:start({local, ?MODULE}, ?MODULE, [], []).

increment_total_monitors() ->
	increment(monitored_items_total).

increment_current_monitors() ->
	increment(monitored_items_current).

decrement_current_monitors() ->
	decrement(monitored_items_current).

increment_unknown_monitors() ->
	increment(monitored_items_unknown).

increment_acked_monitors() ->
	increment(monitored_items_acked).

increment_expired_monitors() ->
	increment(monitored_items_expired).

all_metrics_with_values() ->
	DisplayValues = accumultate_metrics_for_display(stats(),[]),
	DisplayValues.

increment(Name) ->
	folsom_metrics:notify({Name, {inc, 1}}).

decrement(Name) ->
	folsom_metrics:notify({Name, {dec, 1}}).

% list of all stats in form {type, name, displayname, description}
stats() ->
	[	
		{counter, monitored_items_total, <<"Total items">>, <<"Culmitive total of all items monitored.">>},
		{counter, monitored_items_current, <<"Current items">>, <<"Current total items monitored.">>},
		{counter, monitored_items_unknown, <<"Unknown items">>, <<"Unknown items - maybe expired, maybe unknown identifiers.">>},
		{counter, monitored_items_acked, <<"Acked items">>, <<"Total monitored items confirmed.">>},
		{counter, monitored_items_expired, <<"Expired items">>, <<"Total expired items.">>}
	].

iterate_stats(Stat,[]) ->
	create_stat(Stat),
	ok;
iterate_stats(Stat,[H | T]) ->
	create_stat(Stat),
	iterate_stats(H,T).

create_stat({counter,Name,_,_}) ->
	folsom_metrics:new_counter(Name);
create_stat({Type,_}) ->
	lager:error("Unknown stat type ~p.~n", [Type]).

accumultate_metrics_for_display([], Acc) ->
	Acc;
accumultate_metrics_for_display([{_,Name,DisplayName, Description}|T], Acc) ->
	CurrentValue = [{ <<"name">>, DisplayName },{ <<"value">>, folsom_metrics:get_metric_value(Name)},{ <<"description">>, Description}],
	accumultate_metrics_for_display(T, [ CurrentValue | Acc ]).

init([]) ->
	[H|T] = stats(),
	iterate_stats(H,T),
  	{ok, []}.

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