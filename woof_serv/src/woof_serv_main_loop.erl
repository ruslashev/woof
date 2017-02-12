-module(woof_serv_main_loop).
-export([start_link/0]).
-export([init/1]).

start_link() ->
    proc_lib:start_link(woof_serv_main_loop, init, [self()]).

init(Parent) ->
    register(woof_serv_main_loop, self()),
    proc_lib:init_ack(Parent, { ok, self() }),
    ets:new(clients, [set, public, named_table]),
    loop().

loop() ->
    receive

    after 10 ->
        loop()
    end.

