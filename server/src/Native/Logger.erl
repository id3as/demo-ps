-module(logger@foreign).

-export([
         emergency/2,
         alert/2,
         critical/2,
         error/2,
         warning/2,
         notice/2,
         info/2,
         debug/2
        ]).

-define(do_log(Level, Msg, Args),
        [{current_stacktrace, [_LoggerFrame, {Module, Fun, Arity, [{file, File}, {line, Line}]} | _]}] = erlang:process_info(self(), [current_stacktrace]),
        fun() ->
            Location = #{mfa=> {Module, Fun, Arity},
                         line=> Line,
                         file=> File},

            case logger:allow(Level, Module) of
              true ->
                apply(logger, macro_log, [Location, Level, binary_to_list(Msg), Args, #{} ]); %% TODO: re-add metadata here if we actually want it
              false ->
                ok
            end
        end).

emergency(Msg, Args) ->
  ?do_log(emergency, Msg, Args).

alert(Msg, Args) ->
  ?do_log(alert, Msg, Args).

critical(Msg, Args) ->
  ?do_log(critical, Msg, Args).

error(Msg, Args) ->
  ?do_log(error, Msg, Args).

warning(Msg, Args) ->
  ?do_log(warning, Msg, Args).

notice(Msg, Args) ->
  ?do_log(notice, Msg, Args).

info(Msg, Args) ->
  ?do_log(info, Msg, Args).

debug(Msg, Args) ->
  ?do_log(debug, Msg, Args).
