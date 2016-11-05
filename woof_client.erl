-module(woof_client).
-behaviour(gen_server).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).
-record(client, { name, socket }).

-define(PORT_CLIENT, 2710).

start_link(Socket) ->
    gen_server:start_link(?MODULE, Socket, []).

init(Socket) ->
    gen_server:cast(self(), accept),
    { ok, #client{ socket = Socket } }.

handle_cast(accept, State = #client{ socket = Socket }) ->
    woof_client_sup:start_socket(),
    send(Socket, "ohai"),
    { noreply, State };
handle_cast(_, State) ->
    { noreply, State }.

send(Socket, Str) ->
    ok = gen_udp:send(Socket, { 127,0,0,1 }, ?PORT_CLIENT, Str),
    ok = inet:setopts(Socket, [binary, { active, once }]),
    ok.

handle_info({ udp, _Socket, _FromIp, _FromPort, Message }, State) ->
    io:format("got message \"~p\"", [Message]),
    { noreply, State };
handle_info(Unknown, State) ->
    io:format("unexpected: ~p~n", [Unknown]),
    { noreply, State }.

handle_call(_, _, State) ->
    { noreply, State }.

code_change(_OldVsn, State, _Extra) ->
    { ok, State }.

terminate(normal, _State) ->
    ok;
terminate(Reason, _State) ->
    io:format("terminate reason: ~p~n", [Reason]).

