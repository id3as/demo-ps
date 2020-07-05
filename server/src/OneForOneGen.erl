-module(oneForOneGen@foreign).


-export([
         getDataFromSomeNativeCode/0
        ]).

getDataFromSomeNativeCode() ->
  fun() ->
      crypto:strong_rand_bytes(500)
  end.
