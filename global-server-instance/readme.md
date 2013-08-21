compile

windows -
rebar.cmd compile

linux
rebar compile

shell 1

erl -pa ebin -sname node0@localhost

shell 2

erl -pa ebin -sname node1@localhost

shell 3

erl -pa ebin -sname node2@localhost

```
global_instance_app:join_node(node0@localhost).

global_instance_app:join_node(node1@localhost).

global_instance_app:list_nodes().
```

shell 1

```
global_instance_app:start([],[]). <-- see worker here
```

shell 2, shell3

```
global_instance_app:start([],[]). <-- all except for the worker node
```

kill shell 1 

worker starts on shell2 or shell 3

repeat starting, joining and killing

kill shell 2

nothing happens - which is correct.
