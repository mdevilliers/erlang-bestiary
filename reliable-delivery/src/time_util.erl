-module (time_util).

-export ([now_in_seconds/0]).

now_in_seconds() ->
  Now = calendar:local_time(),
  StartTime = calendar:datetime_to_gregorian_seconds(Now),
  StartTime.