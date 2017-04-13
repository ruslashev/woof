-module(woof_serv_main_loop).
-export([start_link/0]).
-export([init/1]).
-include("woof_common.hrl").

start_link() ->
    proc_lib:start_link(woof_serv_main_loop, init, [self()]).

init(Parent) ->
    register(woof_serv_main_loop, self()),
    proc_lib:init_ack(Parent, { ok, self() }),
    ets:new(clients, [set, { keypos, 2 }, public, named_table]),
    ets:new(players, [set, { keypos, 2 }, public, named_table]),
    loop().

loop() ->
    receive
        { new_client, ClientId } ->
            case ets:lookup(players, ClientId) of
                [] -> ets:insert(players, #player{ client_id = ClientId });
                [_ExistingPlayer] -> ok
            end;
        Unknown ->
            io:format("woof_serv_main_loop: unknown msg ~p~n", [Unknown])
    after 10 ->
        loop()
    end.

