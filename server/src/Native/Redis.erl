-module(redis@foreign).

-export([open/1]).

open(ConnectionString) ->
  fun() ->
      { ok, C } = eredis:start_link(ConnectionString),
      C
  end.

