-module(woof_serv_app).
-behaviour(application).
-export([start/2, stop/1]).

start(normal, _) ->
    woof_serv_sup:start_link();
start({ takeover, _ }, _) ->
    woof_serv_sup:start_link().

stop(_) ->
    ok.

