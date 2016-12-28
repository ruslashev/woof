-module(wpool_sup).
-behaviour(supervisor).
-export([start_link/3, init/1]).

start_link(Name, Limit, MFA) ->
    supervisor:start_link(?MODULE, { Name, Limit, MFA }).

init({ Name, Limit, MFA }) ->
    { ok, { { one_for_all, 1, 10 },
            [{ wpool_sup,
              { wpool_sup, start_link, [Name, Limit, self(), MFA] },
              permanent, 5 * 1000, worker, [wpool_sup]
             }
            ]
          }
    }.

