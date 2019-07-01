-module(bookConfig@foreign).

-export([
         readInt_/1,
         readString_/1,
         readDirect_/1
        ]).

%% Case statements are just so we can take env vars as well as sys.config values
readDirect_(Name) -> fun() ->
                      get_config_value(binary_to_atom(Name, utf8))
                     end.

readInt_(Name) -> fun() ->
                      case get_config_value(binary_to_atom(Name, utf8)) of
                        List when is_list(List) -> list_to_integer(List);
                        Int when is_integer(Int) -> Int
                      end
                  end.

readString_(Name) -> fun() ->
                         case get_config_value(binary_to_atom(Name, utf8)) of
                           List when is_list(List) -> list_to_binary(List);
                           Binary when is_binary(Binary) -> Binary
                         end
                  end.

get_config_value(Name) ->
  gproc:get_env(l, demo_ps, Name, [os_env, app_env, error]).

get_config_value(Name, Default) ->
  gproc:get_env(l, demo_ps, Name, [os_env, app_env, {default, Default}]).
