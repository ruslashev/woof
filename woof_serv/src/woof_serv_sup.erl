-module(woof_serv_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({ local, ?MODULE }, ?MODULE, []).

init([]) ->
    { ok, { { one_for_one, 10, 60 },
            [{ woof_serv_sup,
               { woof_serv, start_link, []},
               permanent, 60 * 1000, worker, [woof_serv] }
            ]
          }
    }.

