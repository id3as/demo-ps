-module(simpleBus@foreign).

-export([ subscribe_/2
        , unsubscribe/1
        , raise_/2
        ]).


subscribe_(BusName, Callback) ->
  fun() ->
    Recipient = self(),
    Fun = fun Fun(MonitorRef) ->
              receive
                stop ->
                  gproc:unreg({p, l, BusName}),
                  demonitor(MonitorRef),
                  exit(normal);
                {'DOWN', _, _, _, _} ->
                  gproc:unreg({p, l, BusName}),
                  exit(normal);
                Msg ->
                  Callback(Msg),
                  Fun(MonitorRef)
              end
          end,
    fun() ->
        spawn_link(fun() ->
                       gproc:reg({p, l, BusName}),
                       MonitorRef = monitor(process, Recipient),
                       Fun(MonitorRef) end)
    end
  end.

unsubscribe(Ref) ->
  fun() ->
      Ref ! stop,
      ok
  end.


raise_(BusName, Msg) ->
  fun() ->
    gproc:send({p,l,BusName}, Msg)
  end.
