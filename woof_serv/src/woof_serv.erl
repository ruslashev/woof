-module(woof_serv).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

% -record(client, { client_id = -1, ip, port }).
-record(server_state, { socket, last_client_id = 0, clients = [] }).

-define(ACK, 0).
-define(CONNECTION_REQ, 1).
-define(ERROR, 2).

-define(ERROR_TYPE_OLD_PROTOCOL, 0).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    { ok, PortServer } = application:get_env(port_serv),
    { ok, Socket } = gen_udp:open(PortServer, [binary, { active, once }]),
    { ok, #server_state{ socket = Socket, clients = [] } }.

handle_cast(Unknown, State) ->
    wl:log("unknown cast ~p", [Unknown]),
    { noreply, State }.

handle_info(InfoMsg, State =
            #server_state{ socket = Socket, clients = _Clients }) ->
    case InfoMsg of
        { udp, _Socket, RemoteIp, RemotePort, Message } ->
            RemoteClientTuple = { Socket, RemoteIp, RemotePort },
            wl:log("got message \"~p\"", [Message]),
            <<_Reliable:1, Type:7, Rest/binary>> = Message,
            case Type of
                ?CONNECTION_REQ ->
                    <<_RelMsgId:32, ProtocolVersion:8>> = Rest,
                    % send(RemoteClientTuple, <<?ACK:1, RelMsgId:32>>),
                    if ProtocolVersion =/= 2 ->
                           send(RemoteClientTuple,
                                <<1:1, ?ERROR:7, ?ERROR_TYPE_OLD_PROTOCOL:8>>);
                           % send(RemoteClientTuple, "hi");
                       true ->
                           NewClientId = 123,
                           send(RemoteClientTuple, <<NewClientId:10>>)
                    end;
                _ ->
                    ok
            end;
        Unknown ->
            wl:log("unknown message \"~p\"", [Unknown])
    end,
    inet:setopts(Socket, [{ active, once }]),
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

send({ Socket, RemoteIp, RemotePort }, Msg) ->
    case gen_udp:send(Socket, RemoteIp, RemotePort, Msg) of
        ok -> ok;
        { error, Reason } -> wl:log("Failed to send: ~p", [Reason]), ok
    end.

% new_client_id(State = #server_state { last_client_id = LastClientId }) ->
%     NewClientId = LastClientId + 1,
%     { NewClientId, State#server_state { last_client_id = NewClientId } }.

