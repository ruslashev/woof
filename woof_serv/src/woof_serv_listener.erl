-module(woof_serv_listener).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-define(ACTIVE, { active, true }).
-define(SOCK_OPTS, [binary, ?ACTIVE]).

start_link() ->
    gen_server:start_link({ local, woof_serv_listener }, ?MODULE, [], []).

init([]) ->
    { ok, PortServer } = application:get_env(port_serv),
    case gen_udp:open(PortServer, [binary, ?ACTIVE]) of
        { ok, Socket } ->
            { ok, Socket };
        { error, Reason } ->
            io:format("woof_serv_listener: failed to open socket: ~p~n", [Reason]),
            { stop, Reason }
    end.

handle_info(Message = { udp, _, _, _, _ }, Socket) ->
    woof_serv_handler_sup:handle(Message),
    { noreply, Socket };
handle_info(Unknown, State) ->
    io:format("woof_serv_listener: unknown info: ~p~n", [Unknown]),
    { noreply, State }.

handle_call(Unknown, _From, State) ->
    io:format("woof_serv_listener: unknown call: ~p~n", [Unknown]),
    { noreply, State }.

handle_cast(Unknown, State) ->
    io:format("woof_serv_listener: unknown cast: ~p~n", [Unknown]),
    { noreply, State }.

code_change(_OldVsn, State, _Extra) ->
    { ok, State }.

terminate(Reason, Socket) ->
    io:format("woof_serv_listener: terminate: ~p~n", [Reason]),
    gen_udp:close(Socket).

