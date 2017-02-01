-module(woof_serv_main_loop).
-export([start_link/0]).

-record(client, { client_id, ip }).

start_link() ->
    spawn_link(fun init/0).

init() ->
    ets:new(clients, [set, named_table, public]),
    loop([]).

loop(Clients) ->
    receive

    after 10 ->
        loop(Clients)
    end.

