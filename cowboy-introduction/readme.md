
Compile -

rebar.cmd get-deps

rebar.cmd compile

To run -

erl -pa ebin deps/cowboy/ebin/ deps/ranch/ebin/ -s cowboy_introduction

