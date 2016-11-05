-module(wl).
-export([log/1, log/2]).

log(String) ->
    io:format(String ++ "~n").

log(String, Format) ->
    io:format(String ++ "~n", Format).

