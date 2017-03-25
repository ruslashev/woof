-module(woof_serv_handler).
-export([handle/3]).
-include("woof_common.hrl").

handle(RemoteIp, RemotePort, Packet) ->
    try
        % parse/2 and update/3 should be merged
        ClientId = parse_packet(RemoteIp, Packet),
        io:format("parse~n"),
        update(RemoteIp, ClientId, RemotePort)
    catch
        error:{ badmatch, _ } ->
            io:format("woof_serv_handler: malformed packet~n");
        throw:badarg -> io:format("woof_serv_handler: badarg~n");
        What:E -> io:format("woof_serv_handler: unhandled ~p: ~p~n~p",
                         [What, E, erlang:get_stacktrace()])
    end.

parse_packet(RemoteIp, Packet) ->
    <<_RxReliable:8, RxSequence:32, _RxAck:32, RxClientId:16, RxNumMessages:8,
      RxMessages/binary>> = Packet,
    ClientKey = { RxClientId, RemoteIp },
    case ets:lookup(clients, ClientKey) of
        [] ->
            ets:insert(clients, #client_data{ client_key = ClientKey }),
            parse_packet(RemoteIp, Packet);
        [ClientData = #client_data{
           unacked_packet = #packet{ sequence = ClUnackedPacketSequence },
           last_sequence_received = ClLastSequenceReceived,
           ack_packets = ClAckPackets,
           received_packets = ClReceivedPackets }] ->
            Status = if (RxSequence =:= 0) and (ClLastSequenceReceived =:= 0) ->
                            ok;
                        RxSequence =:= ClLastSequenceReceived + 1 -> ok;
                        RxSequence =/= ClLastSequenceReceived + 1 -> discard
                     end,
            case Status of
                discard ->
                    ets:insert(clients, ClientData#client_data{
                            received_packets = ClReceivedPackets + 1 });
                ok ->
                    if RxSequence =/= ClUnackedPacketSequence ->
                        ets:insert(clients, ClientData#client_data{
                            last_sequence_received = RxSequence,
                            received_packets = ClReceivedPackets + 1 });
                       RxSequence =:= ClUnackedPacketSequence ->
                        ets:insert(clients, ClientData#client_data{
                            last_sequence_received = RxSequence,
                            unacked_packet_exists = false,
                            ack_packets = ClAckPackets + 1,
                            received_packets = ClReceivedPackets + 1 })
                    end,
                    parse_messages(RemoteIp, RxClientId, RxNumMessages,
                                   RxMessages)
            end
    end,
    RxClientId.

parse_messages(_, _, 0, _) ->
    ok;
parse_messages(RemoteIp, ClientId, NumMessages, Messages) ->
    ClientKey = { ClientId, RemoteIp },
    <<Type:8, Rest/binary>> = Messages,
    case Type of
        ?MESSAGE_TYPE_CONNECTION_REQ ->
            <<ProtocolVer:16, NewMessages/binary>> = Rest,
            case ProtocolVer of
                ?PROTOCOL_VERSION ->
                    % todo: not erlangish
                    Response = woof_packet:connection_reply_msg(),
                    io:format("sendin ~p~n", [Response]),
                    woof_packet:send(ClientKey, Response),
                    parse_messages(RemoteIp, ClientId, NumMessages - 1, NewMessages);
                _ -> io:format("woof_serv_handler: wrong protocol version ~p~n",
                             [ProtocolVer])
            end;
        _ ->
            io:format("woof_serv_handler: unhandled message type ~p~n", [Type])
    end.

update(RemoteIp, ClientId, RemotePort) ->
    ClientKey = { ClientId, RemoteIp },
    case ets:lookup(clients, ClientKey) of
        [] ->
            throw(badarg);
        [ClientData = #client_data{
            unrel_messages = ClUnrelMessages,
            messages = ClMessages,
            unacked_packet_exists = ClUnackedPacketExists,
            unacked_packet = ClUnackedPacket,
            outgoing_sequence = ClOutgoingSequence,
            last_sequence_received = ClLastSequenceReceived,
            sent_packets = ClSentPackets }] ->
            ClUnrelMessagesSize = queue:len(ClUnrelMessages),
            ClMessagesSize = queue:len(ClMessages),
            % Throws are a temporary measure
            if ClUnrelMessagesSize > 64 -> throw(unrel_msg_overflow);
               ClMessagesSize > 64 -> throw(msg_overflow);
               true -> ok
            end,
            ClUnrelMessagesNotEmpty = ClUnrelMessagesSize =/= 0,
            ClMessagesNotEmpty = ClMessagesSize =/= 0,
            % Clause actions should be refactored to separate functions
            if ClUnackedPacketExists ->
                FinalPacket = woof_packet:append_msg_queue(ClUnrelMessages,
                        ClUnackedPacket),
                SerializedPacket = woof_packet:serialize(FinalPacket),
                woof_serv_socket:send_binary(RemoteIp, RemotePort,
                        SerializedPacket),
                ets:insert(clients, ClientData#client_data{
                        sent_packets = ClSentPackets + 1 })
             ; ClMessagesNotEmpty ->
                NewPacket = #packet{ reliable = 1,
                                     sequence = ClOutgoingSequence + 1,
                                     ack = ClLastSequenceReceived },
                RelMsgsPacket = woof_packet:append_msg_queue(ClMessages,
                        NewPacket),
                AllMsgsPacket = woof_packet:append_msg_queue(ClUnrelMessages,
                        RelMsgsPacket),
                FinalNumMessages = ClMessagesSize + ClUnrelMessagesSize,
                FinalPacket = AllMsgsPacket#packet{ num_messages =
                        FinalNumMessages },
                SerializedPacket = woof_packet:serialize(FinalPacket),
                woof_serv_socket:send_binary(RemoteIp, RemotePort,
                        SerializedPacket),
                ets:insert(clients, ClientData#client_data{
                        outgoing_sequence = ClOutgoingSequence + 1,
                        sent_packets = ClSentPackets + 1 })
             ; ClUnrelMessagesNotEmpty ->
                NewPacket = #packet{ reliable = 0,
                                     sequence = ClOutgoingSequence + 1,
                                     ack = ClLastSequenceReceived },
                AllMsgsPacket =
                        woof_packet:append_msg_queue_to_packet(
                                ClUnrelMessages, NewPacket),
                FinalNumMessages = ClUnrelMessagesSize,
                FinalPacket = AllMsgsPacket#packet{ num_messages =
                        FinalNumMessages },
                SerializedPacket = woof_packet:serialize(FinalPacket),
                woof_serv_socket:send_binary(RemoteIp, RemotePort,
                        SerializedPacket),
                ets:insert(clients, ClientData#client_data{
                        outgoing_sequence = ClOutgoingSequence + 1,
                        sent_packets = ClSentPackets + 1 })
            end
    end.

