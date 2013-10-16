To start a node


erl -pa ebin \deps\lager\ebin \deps\goldrush\ebin\deps\folsom\ebin \deps\meck\ebin \deps\bear\ebin \deps\locks\ebin \deps\locks\examples\ebin -sname node0@localhost

erl -pa ebin \deps\lager\ebin \deps\goldrush\ebin\deps\folsom\ebin \deps\meck\ebin \deps\bear\ebin \deps\locks\ebin \deps\locks\examples\ebin -sname node1@localhost


To use the example app in locks

```
{ok, DicPid} = gdict:notrace().

gdict:update_counter(a,1,DicPid).

gdict:to_list(DicPid).

```

To join nodes

```

net_kernel:connect(node1@localhost).


```