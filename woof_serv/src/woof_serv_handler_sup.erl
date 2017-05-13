-module(woof_serv_handler_sup).
-behaviour(supervisor).
-export([start_link/0, handle/3]).
-export([init/1]).

start_link() ->
    supervisor:start_link({ global, ?MODULE }, ?MODULE, []).

handle(RemoteIp, RemotePort, Packet) ->
    supervisor:start_child({ global, ?MODULE }, [RemoteIp, RemotePort, Packet]).

init([]) ->
    { ok, { { simple_one_for_one, 1, 1 },
            [{ woof_serv_handler,
               { woof_serv_handler, handle, [] },
               temporary, 1 * 1000, worker, [woof_serv_handler] }]
          }
    }.

