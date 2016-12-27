-module(woof_serv_handler).
-export([start_link/1]).
-export([init/1]).

start_link(Packet) ->
    proc_lib:start_link(?MODULE, init, [[self(), Packet]]).

init([Parent, Packet]) ->
    proc_lib:init_ack(Parent, { ok, self() }),
    answer_query(Packet),
    exit(normal).

answer_query({ udp, _Socket, _RemoteIp, _RemotePort, Packet }) ->
    io:format("Ohai: ~p~n", [Packet]),
    ok.
