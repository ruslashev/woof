-module(woof_serv).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(client, { client_id = -1, ip, port }).
-record(server_state, { socket, clients = [] }).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    { ok, PortServer } = application:get_env(port_serv),
    { ok, Socket } = gen_udp:open(PortServer, [binary, { active, once }]),
    { ok, #server_state{ socket = Socket, clients = [] } }.

handle_cast(Unknown, State) ->
    wl:log("unknown cast ~p", [Unknown]),
    { noreply, State }.

% send(Str) ->
%     ok = gen_tcp:send(Socket, Str ++ "\n"),
%     ok = inet:setopts(Socket, [binary, { active, once }]),
%     ok.

handle_info({ udp, _Socket, RemoteIP, RemotePort, Message },
            State = #server_state{ clients = Clients }) ->
    wl:log("got message \"~p\"", [Message]),
    { noreply, State };
handle_info(Unknown, State) ->
    wl:log("unknown info: ~p", [Unknown]),
    { noreply, State }.

handle_call(Unknown, _, State) ->
    wl:log("unknown call: ~p", [Unknown]),
    { noreply, State }.

code_change(_OldVsn, State, _Extra) ->
    { ok, State }.

terminate(normal, _State = #server_state{ socket = Socket }) ->
    gen_udp:close(Socket),
    ok;
terminate(Reason, State) ->
    wl:log("terminate reason: ~p~n", [Reason]),
    terminate(normal, State).

