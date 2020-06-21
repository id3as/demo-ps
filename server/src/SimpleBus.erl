-module(simpleBus@foreign).


-export([ mapBusMessage/4 ]).


mapBusMessage(Name, Lift, Just, Nothing) ->
  fun(Msg) ->
      case Msg of
        { Name, InnerMsg } -> Just(Lift(InnerMsg));
        _ -> Nothing
      end
  end.
