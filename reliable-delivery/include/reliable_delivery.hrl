

-define (BUCKET_TICK_INTERVAL_MS, 1).
-define (BUCKET_TICKS_PER_BUCKET, 1000).

-record(lease,{ identifier, lease_time, start_time,application }).
-record (monitorvalue, {identifier, pid }).

-record (tick, {start_time , offset = 0 , bucket = 0}).
