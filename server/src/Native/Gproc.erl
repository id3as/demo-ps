-module(gproc@foreign).

-export([ reg/1
        , send/2
        ]).


reg(Name) ->
  fun() ->
    gproc:reg({p, l, { ?MODULE, Name }})
  end.


send(Name, Msg) ->
  fun() ->
    gproc:send({p, l, { ?MODULE, Name }}, { Name, Msg })
  end.
