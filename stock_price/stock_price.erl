% I have an array stock_prices_yesterday where:

% The indices are the time in minutes past trade opening time, which was 9:30am local time.
% The values are the price in dollars of Apple stock at that time.
% For example, the stock cost $500 at 10:30am, so stock_prices_yesterday[60] = 500.

% Write an efficient algorithm for computing the best profit I could have made from 1 purchase and 1 sale of 1 Apple stock yesterday.

% No "shorting"—you must buy before you sell. You may not buy and sell in the same time step (at least 1 minute must pass).

-module (stock_price).

-export ([get_max_profit/1, tests/0]).

-define (MAXLOSS, -99999).
-define (MINPRICE, 99999).

get_max_profit(Data) when length(Data) =:= 1 ->
	not_enough_data;
get_max_profit(Data)  ->
	profit_until_end_of_day(Data, ?MINPRICE, ?MAXLOSS).

profit_until_end_of_day([], _, Profit) ->
	Profit;
profit_until_end_of_day([OpeningPrice|T], MinPrice, Profit) ->

	PotentialProfitPerUnit = OpeningPrice - MinPrice,
	% io:format("Open : ~p MinPrice : ~p Profit : ~p ~n",[OpeningPrice, MinPrice, PotentialProfitPerUnit]),
	profit_until_end_of_day(T, min(MinPrice, OpeningPrice), max(Profit,PotentialProfitPerUnit)).

tests()->
	2 = get_max_profit([1,2,3]),
   -1 = get_max_profit([3,2,1]),
	0 = get_max_profit([1,1,1]),
	3 = get_max_profit([1,2,2,3,3,1,4]),
	
	48 = get_max_profit([10,20,30,50,1,49]),

	15 = get_max_profit(lists:seq(1,16)),
	11519 = get_max_profit(lists:seq(1,60*8*24)),

	not_enough_data = get_max_profit([1]),
	ok.

%  

