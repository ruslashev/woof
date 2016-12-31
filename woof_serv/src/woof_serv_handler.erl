-module(woof_serv_handler).
-export([start_link/1]).

start_link(Packet) ->
    io:format("Packet ~p~n", [Packet]),
    try
        parse_packet(Packet)
    catch error:{ badmatch, _ } ->
              io:format("woof_serv_handler: malformed packet (badmatch)~n");
          _:E ->
              io:format("woof_serv_handler: unhandled exception ~p: ~p~n",
                        [E, erlang:get_stacktrace()])
    end.

parse_packet(Packet) ->
    << Reliable:1, Sequence:31, Ack:32, ClientId:16, NumMessages:8,
       Messages/binary >> = Packet.

