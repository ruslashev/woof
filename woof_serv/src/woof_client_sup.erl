-module(woof_client_sup).
-behaviour(supervisor).
-export([start_link/0, start_socket/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({ local, ?MODULE }, ?MODULE, []).

init([]) ->
    { ok, PortServer } = application:get_env(port_serv),
    { ok, Socket } = gen_tcp:listen(PortServer, [binary, { active, once }]),
    spawn_link(fun empty_sockets/0),
    { ok, { { simple_one_for_one, 100, 1 * 60 },
            [{ socket,
               { woof_client, start_link, [Socket] },
               temporary, 1000, worker, [woof_client] }]
          }
    }.

start_socket() ->
    supervisor:start_child(?MODULE, []).

empty_sockets() ->
    [start_socket() || _ <- lists:seq(1, 2)],
    ok.

