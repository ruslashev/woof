-module(wl).
-export([log/1, log/2, log_nonl/1, log_nonl/2]).

log(String) ->
    io:format(String ++ "~n").

log(String, Format) ->
    io:format(String ++ "~n", Format).

log_nonl(String) ->
    io:format(String).

log_nonl(String, Format) ->
    io:format(String, Format).

