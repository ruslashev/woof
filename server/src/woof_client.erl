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

handle_call(_, _, State) ->
    { noreply, State }.

handle_cast(accept, S = #client{ socket = Socket }) ->
    { ok, AcceptSocket } = gen_tcp:accept(Socket),
    woof_client_sup:start_socket(),
    send(AcceptSocket, "hello ddue", []),
    { noreply, S#client{ socket = AcceptSocket } };
handle_cast(_, State) ->
    { noreply, State }.

send(Socket, Str, Args) ->
    ok = gen_tcp:send(Socket, io_lib:format(Str++"~n", Args)),
    ok = inet:setopts(Socket, [{ active, once }]),
    ok.

handle_info({ tcp, _Socket, Str }, State) ->
    Name = hd(string:tokens(Str, "\r\n ")),
    gen_server:cast(self(), roll_stats),
    { noreply, State#client{ name = Name } };
handle_info({ tcp_closed, _Socket }, State) ->
    { stop, normal, State };
handle_info({ tcp_error, _Socket, _ }, State) ->
    { stop, normal, State };
handle_info(Unknown, State) ->
    io:format("unexpected: ~p~n", [Unknown]),
    { noreply, State }.

code_change(_OldVsn, State, _Extra) ->
    { ok, State }.

terminate(normal, _State) ->
    ok;
terminate(Reason, _State) ->
    io:format("terminate reason: ~p~n", [Reason]).

