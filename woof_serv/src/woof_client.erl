-module(woof_client).
-behaviour(gen_server).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-record(client, { name, socket }).

start_link(Socket) ->
    gen_server:start_link(?MODULE, Socket, []).

init(Socket) ->
    gen_server:cast(self(), accept),
    { ok, #client{ socket = Socket } }.

handle_cast(accept, State = #client{ socket = Socket }) ->
    { ok, AcceptSocket } = gen_tcp:accept(Socket),
    woof_client_sup:start_socket(),
    send(AcceptSocket, "ohai"),
    { noreply, State };
handle_cast(Unknown, State) ->
    wl:log("unknown cast ~p", [Unknown]),
    { noreply, State }.

send(Socket, Str) ->
    % { ok, PortClient } = application:get_env(port_client),
    ok = gen_tcp:send(Socket, Str ++ "\n"),
    ok = inet:setopts(Socket, [binary, { active, once }]),
    ok.

% handle_info({ udp, _Socket, _FromIp, _FromPort, Message }, State) ->
%     wl:log("got message \"~p\"", [Message]),
%     { noreply, State };
handle_info({ tcp, _Socket, Message }, State) ->
    wl:log("got message \"~p\"", [Message]),
    { noreply, State };
handle_info({ tcp_closed, _Socket }, S) ->
    { stop, normal, S };
handle_info({ tcp_error, _Socket, _ }, S) ->
    { stop, normal, S };
handle_info(Unknown, State) ->
    wl:log("unknown info: ~p", [Unknown]),
    { noreply, State }.

handle_call(_, _, State) ->
    { noreply, State }.

code_change(_OldVsn, State, _Extra) ->
    { ok, State }.

terminate(normal, _State) ->
    ok;
terminate(Reason, _State) ->
    wl:log("terminate reason: ~p~n", [Reason]).

