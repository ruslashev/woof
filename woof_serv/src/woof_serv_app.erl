-module(woof_serv_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_, _) ->
    woof_serv_sup:start_link(),
    woof_client_sup:start_link().

stop(_) ->
    ok.

