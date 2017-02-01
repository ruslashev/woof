-module(woof_serv_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_, _) ->
    woof_serv_supersup:start_link(),
    woof_serv_main_loop:start_link().

stop(_) ->
    ok.

