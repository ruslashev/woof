-module(woof_serv_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({ local, ?MODULE }, ?MODULE, []).

init([]) ->
    { ok, { { one_for_one, 10, 60 },
            [{ woof_client_sup,
               { woof_client_sup, start_link, []},
               permanent, 60 * 1000, supervisor, [woof_client_sup] }
            ]
          }
    }.

