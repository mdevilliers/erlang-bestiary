Get going
---------

rebar.cmd get-deps
rebar.cmd compile


Compile just this project
-------------------------

rebar.cmd compile skip_deps=true


Run on windows
--------------


erl -pa ebin \deps\lager\ebin \deps\goldrush\ebin \deps\folsom\ebin \deps\meck\ebin \deps\bear\ebin \deps\ranch\ebin \deps\cowboy\ebin deps\jsx\ebin deps\dh_date\ebin deps\mimetypes\ebin deps\bullet\ebin deps\uuid\ebin deps\quickrand\ebin deps\gproc\ebin deps\eredis\ebin -s reliable_delivery

Run on linux
-------------

erl -pa ebin deps/*/ebin -s reliable_delivery


Demo api
--------

```
reliable_delivery:start().
{ok, Identifier} =  reliable_delivery:monitor(5000,<<"my application name">>, <<"my value">>).

% then

reliable_delivery:ack(Identifier).



```

Run test harness
----------------

```
test_harness:start(1).
```


Test web api
--------

curl -i -H "Accept: application/json" -X GET http://localhost:8180/api/statistics

