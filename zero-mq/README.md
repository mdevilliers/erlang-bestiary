rebar get-deps

rebar compile

run the server

erl -pa ebin deps/*/ebin



run the client

escript ./src/subscriber_client.es