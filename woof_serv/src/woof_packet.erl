-module(woof_packet).
-export([serialize/1]).
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

append_msg_queue(MsgQueue, #packet{
        serialized_messages = SerializedMessages }) ->
    #packet { serialized_messages = }

