
windows

erl -pa ebin \deps\lager\ebin \deps\goldrush\ebin \deps\folsom\ebin \deps\meck\ebin \deps\bear\ebin \deps\ranch\ebin \deps\cowboy\ebin deps\jsx\ebin -s reliable_delivery

linux

erl -pa ebin deps/*/ebin -s reliable_delivery

```
reliable_delivery:start().
reliable_delivery:monitor(<<"myuniqueidentifier">>,5000,<<"my value">>).

message_store:lookup(c).

% or 

reliable_delivery:ack(<<"myuniqueidentifier">>).

```

Test api
--------

curl -i -H "Accept: application/json" -X GET http://localhost:8180/api/state

