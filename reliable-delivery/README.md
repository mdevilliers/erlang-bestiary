
windows

erl -pa ebin \deps\lager\ebin \deps\goldrush\ebin \deps\folsom\ebin \deps\meck\ebin \deps\bear\ebin

linux

erl -pa ebin deps/*/ebin

```
reliable_delivery:start().
reliable_delivery:monitor(<<"myuniqueidentifier">>,5000,<<"my value">>).

message_store:lookup(c).

% or 

reliable_delivery:ack(<<"myuniqueidentifier">>).

```

