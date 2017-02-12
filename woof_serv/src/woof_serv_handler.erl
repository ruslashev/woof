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
    <<_RxReliable:8, RxSequence:32, RxAck:32, RxClientId:16, _RxNumMessages:8,
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

