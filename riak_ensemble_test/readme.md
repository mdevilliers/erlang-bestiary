Get going
---------

rebar.cmd get-deps
rebar.cmd compile


Compile just this project
-------------------------

rebar.cmd compile skip_deps=true


Run on windows
--------------


erl -pa ebin deps\lager\ebin deps\goldrush\ebin deps\riak_ensemble\ebin

Run on linux
-------------

erl -pa ebin deps/*/ebin
