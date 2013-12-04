-module (reliable_delivery_event).

-export ([notify/2, subscribe/1]).

notify(EventType, Msg) ->
    Key = {EventType},
    gproc:send({p, l, Key}, { self(), Key, Msg }).

subscribe(Event) ->
    gproc:reg({p, l, { Event }}).