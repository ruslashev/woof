-module(woof_serv).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(client, { client_id, ip }).
-record(server_state, { socket, clients }).

-define(PROTOCOL_VERSION, 1).

-define(MESSAGE_TYPE_ACK, 0).
-define(MESSAGE_TYPE_PING, 1).
-define(MESSAGE_TYPE_CONNECTION_REQ, 2).

-define(SERVER_MESSAGE_TYPE_ACK, 0).
-define(SERVER_MESSAGE_TYPE_PONG, 1).
-define(SERVER_MESSAGE_TYPE_CONNECTION_REPLY, 2).
-define(SERVER_MESSAGE_TYPE_ERROR, 3).

-define(ERROR_TYPE_NOT_MATCHING_PROTOCOL, 0).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    { ok, PortServer } = application:get_env(port_serv),
    case gen_udp:open(PortServer, [binary, { active, once }]) of
        { ok, Socket } ->
            { ok, #server_state{ socket = Socket, clients = sets:new() } };
        { error, Reason } ->
            wl:log("Failed to open server socket: ~p", [Reason]),
            { stop, Reason }
    end.

handle_info(InfoMsg, State = #server_state{ socket = Socket }) ->
    case InfoMsg of
        { udp, _Socket, RemoteIp, RemotePort, Packet } ->
            ClientTuple = { Socket, RemoteIp, RemotePort },
            wl:log("got packet \"~p\"~nfrom ~p", [Packet, ClientTuple]),
            try handle_udp_packet(ClientTuple, Packet, State)
            catch
                error:{ badmatch, _ } -> wl:log("Malformed packet");
                _:Exc -> wl:log("Unhandled exception from handle_udp_packet:"
                                " ~p~n~p", [Exc, erlang:get_stacktrace()])
            end;
        Unknown ->
            wl:log("unknown info \"~p\"", [Unknown])
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

handle_udp_packet(ClientTuple = { _Socket, RemoteIp, _RemotePort }, Packet,
                  State = #server_state{ clients = Clients }) ->
    <<_Reliable:1, _IncomingSequence:31, _AckSequence:32, ClientId:16,
      NumMessages:8, Messages/binary>> = Packet,
    ClientRec = #client { client_id = ClientId, ip = RemoteIp },
    case sets:is_element(ClientRec, Clients) of
        false ->

        true ->

    handle_message(ClientTuple, NumMessages, Messages, State).

handle_message(_, 0, _, _) -> ok;
handle_message(ClientTuple, NumMessages, Messages, State) ->
    <<Type:8, Rest/binary>> = Messages,
    case Type of
        ?MESSAGE_TYPE_PING ->
            <<TimeSent:32, RestMessages/binary>> = Rest,
            send(ClientTuple, <<?SERVER_MESSAGE_TYPE_PONG:8, TimeSent:32>>),
            handle_message(ClientTuple, NumMessages - 1, RestMessages, State);
        ?MESSAGE_TYPE_CONNECTION_REQ ->
            <<ProtocolVer:16, RestMessages/binary>> = Rest,
            case ProtocolVer of
                ?PROTOCOL_VERSION ->
                    ok;
                _ ->
                    ok
            end,
            handle_message(ClientTuple, NumMessages - 1, RestMessages, State)
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

