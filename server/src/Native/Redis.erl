-module(redis@foreign).

-define(SET(Key, Value), [ <<"SET">>, Key, Value ]).
-define(GET(Key), [ <<"GET">>, Key ]).
-define(KEYS(Match), [ <<"KEYS">>, Match ]).
-define(MGET(Keys), [ <<"MGET">> | Keys ]).
-define(DEL(Key), [ <<"DEL">> | if is_list(Key) -> Key; true -> [ Key ] end ]).

-export([open/1,
         put_/3,
         get_/4,
         readKeyPrefix_/2,
         delete/2
        ]).

open(ConnectionString) ->
  fun() ->
      { ok, C } = eredis:start_link(ConnectionString),
      C
  end.

put_(Id, Data, Pid) ->
  fun() ->
      { ok, <<"OK">>} = eredis:q(Pid, ?SET(Id, Data)),
      ok
  end.

delete(Id, Pid) ->
  fun() ->
      { ok, _Count } = eredis:q(Pid, ?DEL(Id)),
      ok
  end.

get_(Id, Pid, Nothing, Just) ->
  fun() ->
      case eredis:q(Pid, ?GET(Id)) of
        {ok, undefined} -> Nothing;
        {ok, Binary} -> Just(Binary)
      end
  end.

readKeyPrefix_(Prefix, Pid) ->
  fun() ->
      {ok, Keys} = eredis:q(Pid, ?KEYS(<<Prefix/binary, "*">>)),
      {ok, Binaries} = mget(Keys, Pid),
      Binaries
  end.

mget([], _Connection) ->
  { ok, [] };
mget(Keys, Connection) ->
  eredis:q(Connection, ?MGET(Keys)).
