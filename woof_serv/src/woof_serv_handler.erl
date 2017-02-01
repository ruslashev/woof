-module(woof_serv_handler).
-export([handle/1]).

handle(Packet) ->
    try
        io:format("processing packet: ~p~n", [Packet]),
        parse(Packet)
    catch
        error:{ badmatch, _ } -> io:format("woof_serv_handler: malformed packet");
        _:E -> io:format("woof_serv_handler: unhandled exception:  ~p~n~p",
                         [E, erlang:get_stacktrace()])
    end.

parse(Packet) ->
    <<_Reliable:8, _Sequence:31, _Ack:32, _ClientId:16, _NumMessages:8,
      _Messages/binary>> = Packet.

