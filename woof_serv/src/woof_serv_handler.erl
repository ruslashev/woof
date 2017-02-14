-module(woof_serv_handler).
-export([handle/2]).
-include("woof_common.hrl").

handle(RemoteIp, Packet) ->
    try
        parse(RemoteIp, Packet),
        io:format("parsed packet: ~p~n", [Packet])
    catch
        error:{ badmatch, _ } -> io:format("woof_serv_handler: malformed packet~n");
        _:E -> io:format("woof_serv_handler: unhandled exception:  ~p~n~p",
                         [E, erlang:get_stacktrace()])
    end.

parse(RemoteIp, Packet) ->
    <<_RxReliable:8, RxSequence:32, _RxAck:32, RxClientId:16, _RxNumMessages:8,
      _RxMessages/binary>> = Packet,
    ClientKey = { RxClientId, RemoteIp },
    case ets:lookup(clients, ClientKey) of
        [] ->
            ets:insert(clients, #client_data{ client_key = ClientKey });
        [ClientData = #client_data{
           unacked_packet = #packet{ sequence = ClUnackedPacketSequence },
           last_sequence_received = ClLastSequenceReceived,
           ack_packets = ClAckPackets,
           received_packets = ClReceivedPackets }] ->
            Status = if (RxSequence =:= 0) and (ClLastSequenceReceived =:= 0) -> ok;
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
                    end
            end
    end.

send(RemoteIp, ClientId) ->
    ClientKey = { ClientId, RemoteIp },
    case ets:lookup(clients, ClientKey) of
        [] ->
            erlang:error(badarg);
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
            if ClUnrelMessagesSize > 64 -> erlang:error(unrel_msg_overflow);
               ClMessagesSize > 64 -> erlang:error(msg_overflow);
               true -> ok
            end,
            ClUnrelMessagesNotEmpty = ClUnrelMessagesSize =/= 0,
            ClMessagesNotEmpty = ClMessagesSize =/= 0,
            if ClUnackedPacketExists or ClUnrelMessagesNotEmpty or
               ClMessagesNotEmpty ->
                if ClUnackedPacketExists ->
                    FinalPacket = woof_packet:append_msg_queue(ClUnrelMessages,
                            ClUnackedPacket),
                    SerializedPacket = woof_packet:serialize(FinalPacket)
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
                    ets:insert(clients, ClientData#client_data{
                            outgoing_sequence = ClOutgoingSequence + 1,
                            sent_packets = ClSentPackets + 1 })
                 ; ClUnrelMessagesNotEmpty ->
                    NewPacket = #packet{ reliable = 0,
                                         sequence = ClOutgoingSequence + 1,
                                         ack = ClLastSequenceReceived },
                    AllMsgsPacket = woof_packet:append_msg_queue(ClUnrelMessages,
                            NewPacket),
                    FinalNumMessages = ClUnrelMessagesSize,
                    FinalPacket = AllMsgsPacket#packet{ num_messages =
                            FinalNumMessages },
                    SerializedPacket = woof_packet:serialize(FinalPacket),
                    ets:insert(clients, ClientData#client_data{
                            outgoing_sequence = ClOutgoingSequence + 1,
                            sent_packets = ClSentPackets + 1 })
                end
             ; true -> nothing_to_send
            end
    end.

