
-define (BUCKET_TIMESPAN_MS, 1000).

-record(lease,{ identifier, lease_time, start_time,application }).
-record (monitorvalue, {identifier, pid, value, created, timeout }).

