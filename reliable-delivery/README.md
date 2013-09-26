
erl -pa ebin deps/*/ebin

```
reliable_delivery:start().
reliable_delivery:monitor(c,5000,<<"aa">>).
message_store:lookup(c).
```

