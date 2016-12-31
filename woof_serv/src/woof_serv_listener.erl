-module(woof_serv_listener).
-export([start_link/0, init/1]).

-define(ACTIVE, { active, once }).
-define(SOCK_OPTS, [binary, ?ACTIVE]).

start_link() ->
    spawn_link({ local, woof_serv_listener }, ?MODULE, init, []).

init([]) ->
    { ok, PortServer } = application:get_env(port_serv),
    case gen_udp:open(PortServer, ?SOCK_OPTS) of
        { ok, Socket } ->
            loop(Socket);
        { error, Reason } ->
            io:format("Failed to open socket: ~p~n", [Reason]),
            exit(Reason)
    end.

loop(Socket) ->
    inet:setopts(Socket, ?SOCK_OPTS),
    receive
        Message = { udp, _, _, _, _ } ->
            woof_serv_handler_sup:handle(Message);
        terminate ->
            gen_udp:close(Socket),
            exit(normal);
        Unknown ->
            io:format("woof_serv_listener: unknown info: ~p~n", [Unknown])
    end,
    loop(Socket).

