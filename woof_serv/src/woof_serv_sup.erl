-module(woof_serv_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({ global, ?MODULE }, ?MODULE, []).

init([]) ->
    { ok, { { one_for_one, 5, 10 },
            [{ woof_serv_socket,
               { woof_serv_socket, start_link, [] },
               permanent, 1 * 1000, worker, [woof_serv_socket] },
             { woof_serv_main_loop,
               { woof_serv_main_loop, start_link, [] },
               permanent, 1 * 1000, worker, [woof_serv_main_loop] },
             { woof_serv_handler_sup,
               { woof_serv_handler_sup, start_link, [] },
               permanent, 1 * 1000, supervisor, [woof_serv_handler_sup] }
            ]
          }
    }.

