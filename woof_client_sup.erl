-module(woof_client_sup).
-behaviour(supervisor).
-export([start_link/0, start_socket/0]).
-export([init/1]).

-define(PORT_SERV, 2711).

start_link() ->
    supervisor:start_link({ local, ?MODULE }, ?MODULE, []).

init([]) ->
    { ok, Socket } = gen_udp:open(?PORT_SERV, [binary, { active, once }]),
    spawn_link(fun empty_sockets/0),
    { ok, { { simple_one_for_one, 100, 5 * 60 },
            [{ socket,
               { woof_client, start_link, [Socket] },
               temporary, 1000, worker, [woof_client] }] } }.

start_socket() ->
    supervisor:start_child(?MODULE, []).

empty_sockets() ->
    [start_socket() || _ <- lists:seq(1, 20)],
    ok.

