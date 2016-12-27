-module(woof_serv_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({ local, ?MODULE }, ?MODULE, []).

init([]) ->
    { ok, { { one_for_one, 5, 10 },
            [{ woof_serv_listener,
               { woof_serv_listener, start_link, [] },
               permanent, 5 * 1000, worker, [woof_serv_listener] },
             { woof_serv_handler_sup,
               { woof_serv_handler_sup, start_link, [] },
               permanent, 5 * 1000, supervisor, [woof_serv_handler_sup] }
            ]
          }
    }.

