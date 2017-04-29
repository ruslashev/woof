%%% woof_utils: auxillary functions for working with packets and message queues
-module(woof_utils).
-export([serialize/1, append_msg_queue_to_packet/2, send/2, send_rel/2,
         connection_reply_msg/0, pong_msg/1, update_msg/1,
         generate_random_color/0]).
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
              NumMessages:8/big, SerializedMessages/binary-big>>
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

connection_reply_msg() ->
    Type = ?SERVER_MESSAGE_TYPE_CONNECTION_REPLY,
    <<Type:8>>.

pong_msg(TimeSent) ->
    Type = ?SERVER_MESSAGE_TYPE_PONG,
    <<Type:8, TimeSent:32>>.

update_msg(Players) ->
    Type = ?SERVER_MESSAGE_TYPE_UPDATE,
    lists:foldl(fun(#player{ position_x = PositionX,
                             position_y = PositionY,
                             alive = Alive,
                             color = { ColorR, ColorG, ColorB } }, Acc) ->
                    AliveInt = case Alive of
                                   true -> 1;
                                   false -> 0
                               end,
                    <<Acc/binary, PositionX:16, PositionY:16, AliveInt:8,
                      ColorR:8, ColorG:8, ColorB:8>>
                end, <<Type:8>>, Players).

generate_random_color() ->
    H = 59 + rand:uniform(241),
    S = 45,
    V = 87,
    hsv_to_rgb(H, S, V).

hsv_to_rgb(Hb, Sb, Vb) ->
    H = Hb / 360.0,
    S = Sb / 100.0,
    V = 255.0 * Vb / 100.0,
    I = trunc(H * 6),
    F = H * 6 - I,
    P = V * (1 - S),
    Q = V * (1 - F * S),
    T = V * (1 - (1 - F) * S),
    { Rf, Gf, Bf } =
        case I rem 6 of
            0 -> { V, T, P };
            1 -> { Q, V, P };
            2 -> { P, V, T };
            3 -> { P, Q, V };
            4 -> { T, P, V };
            5 -> { V, P, Q }
        end,
    { round(Rf), round(Gf), round(Bf) }.

