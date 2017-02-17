-module(woof_packet).
-export([serialize/1, append_msg_queue/2]).
-include("woof_common.hrl").

serialize(#packet{
        reliable = Reliable,
        sequence = Sequence,
        ack = Ack,
        client_id = ClientId,
        num_messages = NumMessages,
        serialized_messages = SerializedMessages }) ->
    <<Reliable:8/big, Sequence:32/big, Ack:32/big, ClientId:16/big,
      NumMessages:8/big, SerializedMessages/big>>.

append_msg_queue(MsgQueue, Packet = #packet{
        serialized_messages = SerializedMessages }) ->
    case queue:out(MsgQueue) of
        { empty, _ } ->
            Packet;
        { { value, HeadMsg }, NewQueue } ->
            NewPacket = #packet{ serialized_messages =
                    <<SerializedMessages/binary, HeadMsg/binary>> },
            append_msg_queue(NewQueue, NewPacket)
    end.

