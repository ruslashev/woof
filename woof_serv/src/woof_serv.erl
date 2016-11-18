-module(woof_serv).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

% -record(client, { client_id = -1, ip, port }).
-record(server_state, { socket, last_client_id = 0, clients = [] }).

-define(PROTOCOL_VERSION, 1).

-define(PACKET_TYPE_ACK, 0).
-define(PACKET_TYPE_CONNECTION_REQ, 1).

-define(SERVER_PACKET_TYPE_ACK, 0).
-define(SERVER_PACKET_TYPE_PONG, 1).
-define(SERVER_PACKET_TYPE_CONNECTION_REPLY, 2).
-define(SERVER_PACKET_TYPE_ERROR, 3).

-define(ERROR_TYPE_NOT_MATCHING_PROTOCOL, 0).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    { ok, PortServer } = application:get_env(port_serv),
    case gen_udp:open(PortServer, [binary, { active, once }]) of
        { ok, Socket } ->
            { ok, #server_state{ socket = Socket } };
        { error, Reason } ->
            wl:log("Failed to open server socket: ~p", [Reason]),
            { stop, Reason }
    end.

handle_info(InfoMsg, State = #server_state{ socket = Socket }) ->
    case InfoMsg of
        { udp, _Socket, RemoteIp, RemotePort, Message } ->
            ClientTuple = { Socket, RemoteIp, RemotePort },
            wl:log("got message \"~p\" from ~p", [Message, ClientTuple]),
            try handle_udp_message(ClientTuple, Message)
            catch
                error:{ badmatch, _ } -> wl:log("Malformed message");
                _:Exc -> wl:log("Unhandled exception from handle_udp_message:"
                                " ~p~n~p", [Exc, erlang:get_stacktrace()])
            end;
        Unknown ->
            wl:log("unknown message \"~p\"", [Unknown])
    end,
    inet:setopts(Socket, [{ active, once }]),
    { noreply, State }.

handle_cast(Unknown, State) ->
    wl:log("unknown cast ~p", [Unknown]),
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

handle_udp_message(ClientTuple, Message) ->
    <<Type:7, _Reliable:1, Rest/binary>> = Message,
    case Type of
        ?PACKET_TYPE_CONNECTION_REQ ->
            <<_RelMsgId:32, ProtocolVersion:8>> = Rest,
            case ProtocolVersion =:= 2 of
                true ->
                    NewClientId = 13435,
                    wl:log("Connection req from client ~p. Its client id is now"
                           " ~p", [ClientTuple, NewClientId]),
                    send(ClientTuple, <<?SERVER_PACKET_TYPE_CONNECTION_REPLY:7,
                                        1:1, NewClientId:16>>);
                false ->
                    wl:log("Connection req from client ~p. Its protocol version"
                           " (~p) does equal current (~p)",
                           [ClientTuple, ProtocolVersion, ?PROTOCOL_VERSION]),
                    send(ClientTuple, <<?SERVER_PACKET_TYPE_ERROR:7, 1:1,
                                        ?ERROR_TYPE_NOT_MATCHING_PROTOCOL:8>>)
            end;
        _ ->
            wl:log("Bad message type ~p from client ~p", [Type, ClientTuple])
    end.

send({ Socket, RemoteIp, RemotePort }, Msg) ->
    case gen_udp:send(Socket, RemoteIp, RemotePort, Msg) of
        ok -> ok;
        { error, Reason } ->
            wl:log("Failed to send: ~p", [Reason]),
            erlang:error(Reason)
    end.

% new_client_id(State = #server_state { last_client_id = LastClientId }) ->
%     NewClientId = LastClientId + 1,
%     { NewClientId, State#server_state { last_client_id = NewClientId } }.

