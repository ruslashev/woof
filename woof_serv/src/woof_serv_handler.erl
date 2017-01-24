-module(woof_serv_handler).
-export([start_link/1]).

start_link(Packet) ->
    io:format("Packet ~p~n", [Packet]),
	io:nl().
    % proc_lib:start_link(?MODULE, init, [[self(), Packet]]).

