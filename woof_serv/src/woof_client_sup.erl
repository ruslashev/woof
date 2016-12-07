-module(woof_client_sup).
-behaviour(supervisor).
-export([start_link/0, start_socket/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({ local, ?MODULE }, ?MODULE, []).

init([]) ->
    { ok, { { simple_one_for_one, 100, 1 * 60 },
            [{ woof_client_sup,
               { woof_client, start_link, [] },
               temporary, 1000, worker, [woof_client] }]
          }
    }.

start_socket() ->
    supervisor:start_child(?MODULE, []).

