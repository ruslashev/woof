%%% woof_packet: auxillary functions for working with packets and message queues
-module(woof_packet).
-export([serialize/1, append_msg_queue_to_packet/2, send/2, send_rel/2]).
-include("woof_common.hrl").

serialize(#packet{
        reliable = Reliable,
        sequence = Sequence,
        ack = Ack,
        client_id = ClientId,
        num_messages = NumMessages,
        serialized_messages = SerializedMessages }) ->
    case SerializedMessages of
        undefined ->
            <<Reliable:8/big, Sequence:32/big, Ack:32/big, ClientId:16/big,
              NumMessages:8/big>>;
        _ ->
            <<Reliable:8/big, Sequence:32/big, Ack:32/big, ClientId:16/big,
              NumMessages:8/big, SerializedMessages>>
    end.

append_msg_queue_to_packet(MsgQueue, Packet = #packet{
        serialized_messages = SerializedMessages }) ->
    case queue:out(MsgQueue) of
        { empty, _ } ->
            Packet;
        { { value, HeadMsg }, NewQueue } ->
            case SerializedMessages of
                undefined ->
                    NewPacket = Packet#packet{ serialized_messages =
                            <<HeadMsg/binary-big>> },
                    append_msg_queue_to_packet(NewQueue, NewPacket);
                _ ->
                    NewPacket = Packet#packet{ serialized_messages =
                            <<SerializedMessages/binary, HeadMsg/binary-big>> },
                    append_msg_queue_to_packet(NewQueue, NewPacket)
            end
    end.

send(ClientKey, Message) ->
    % ets:update_element (?)
    case ets:lookup(clients, ClientKey) of
        [] -> throw(badarg);
        [ClientData = #client_data{
           unrel_messages = ClUnrelMessages }] ->
            ClUnrelMessages2 = queue:in(Message, ClUnrelMessages),
            ets:insert(clients, ClientData#client_data{
                    unrel_messages = ClUnrelMessages2 })
    end.

send_rel(ClientKey, Message) ->
    case ets:lookup(clients, ClientKey) of
        [] -> throw(badarg);
        [ClientData = #client_data{ messages = ClMessages }] ->
            ClMessages2 = queue:in(Message, ClMessages),
            ets:insert(clients, ClientData#client_data{
                    messages = ClMessages2 })
    end.

