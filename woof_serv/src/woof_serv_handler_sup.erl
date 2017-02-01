-module(woof_serv_handler_sup).
-behaviour(supervisor).
-export([start_link/0, handle/1]).
-export([init/1]).

start_link() ->
    supervisor:start_link({ local, woof_serv_handler_sup }, ?MODULE, []).

handle(Packet) ->
    supervisor:start_child(?MODULE, [Packet]).

init([]) ->
    { ok, { { simple_one_for_one, 5, 10 },
            [{ woof_serv_handler,
               { woof_serv_handler, handle, [] },
               permanent, 5 * 1000, worker, [woof_serv_handler] }]
          }
    }.

