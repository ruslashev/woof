-module(wpool_worker_sup).
-behaviour(supervisor).
-export([start_link/1, init/1]).

start_link(MFA) ->
    supervisor:start_link(?MODULE, MFA).

init({ M, F, A }) ->
    { ok, { { simple_one_for_one, 3, 5 },
            [{ wpool_worker, { M,F,A },
               temporary, 3000, worker, [M] }
            ]
          }
    }.

