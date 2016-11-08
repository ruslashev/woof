-module(woof_client).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    { ok, [] }.

handle_cast(Unknown, State) ->
    wl:log("unknown cast ~p", [Unknown]),
    { noreply, State }.

handle_info(Unknown, State) ->
    wl:log("unknown info: ~p", [Unknown]),
    { noreply, State }.

handle_call(_, _, State) ->
    { noreply, State }.

code_change(_OldVsn, State, _Extra) ->
    { ok, State }.

terminate(normal, _State) ->
    ok;
terminate(Reason, _State) ->
    wl:log("terminate reason: ~p~n", [Reason]),
    ok.

