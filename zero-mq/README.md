rebar get-deps

rebar compile

run the server

erl -pa ebin deps/*/ebin

```

zero_mq_app:start([],[]).
zero_mq_publisher:listen().

```

run the client

escript ./src/subscriber_client.es