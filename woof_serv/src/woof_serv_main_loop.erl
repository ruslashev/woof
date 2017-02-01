-module(woof_serv_main_loop).
-export([start_link/0]).
-export([init/1]).

-record(client, { client_id, ip }).

start_link() ->
    proc_lib:start_link(woof_serv_main_loop, init, [self()]).

init(Parent) ->
    register(woof_serv_main_loop, self()),
    proc_lib:init_ack(Parent, { ok, self() }),
    ets:new(clients, [set, named_table, public]),
    loop().

loop() ->
    receive

    after 10 ->
        loop()
    end.

