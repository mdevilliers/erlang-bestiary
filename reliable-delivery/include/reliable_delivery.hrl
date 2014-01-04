
% bucket manager
-define (BUCKET_TICK_INTERVAL_MS, 1).
-define (BUCKET_TICKS_PER_BUCKET, 1000).
-record (tick, {start_time , offset = 0 , bucket = 0}).

% monitor 
-record (lease,{ identifier, lease_time, start_time,application }).

% monitor store
-record (monitorvalue, {identifier, pid }).

% bucket store lite
-record (bucket_monitor_state, {identifier, bucket, state }).
-record (bucket_monitor, {bucket, identifier }).
-record (monitor, {identifier, offsetInBucket, leaseTime, application, value }).
