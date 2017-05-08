-module(woof_serv_main_loop).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).
-include("woof_common.hrl").

start_link() ->
    gen_server:start_link({ local, woof_serv_main_loop }, ?MODULE, [], []).

init([]) ->
    ets:new(clients, [set, { keypos, 2 }, public, named_table]),
    Timer = erlang:send_after(1, self(), tick),
    { ok, Timer }.

handle_info(tick, Timer) ->
    erlang:cancel_timer(Timer),
    timeout_clients(),
    Clients = ets:tab2list(clients),
    if Clients =/= [] ->
        send_update_to_clients(Clients, woof_utils:update_msg(Clients));
       true -> ok
    end,
    NewTimer = erlang:send_after(?SERVER_UPDATE_SEND_DELAY_MS, self(), tick),
    { noreply, NewTimer };
handle_info(Unknown, State) ->
    io:format("woof_serv_main_loop: unknown info: ~p~n", [Unknown]),
    { noreply, State }.

handle_call(Unknown, _From, State) ->
    io:format("woof_serv_main_loop: unknown call: ~p~n", [Unknown]),
    { noreply, State }.

handle_cast(Unknown, State) ->
    io:format("woof_serv_main_loop: unknown cast: ~p~n", [Unknown]),
    { noreply, State }.

code_change(_OldVsn, State, _Extra) ->
    { ok, State }.

terminate(Reason, _State) ->
    io:format("woof_serv_main_loop: terminate: ~p~n", [Reason]).

send_update_to_clients([], _) ->
    ok;
send_update_to_clients([#client_data{ client_id = ClientId } | TailClients],
                       UpdateMsg) ->
    woof_utils:send(ClientId, UpdateMsg),
    send_update_to_clients(TailClients, UpdateMsg).

timeout_clients() ->
    ets:safe_fixtable(clients, true),
    timeout_clients(ets:first(clients)),
    ets:safe_fixtable(clients, false).

timeout_clients('$end_of_table') ->
    ok;
timeout_clients(Key) ->
    % 13 - keypos for time_since_last_ping
    ets:update_counter(clients, Key, { 13, ?SERVER_UPDATE_SEND_DELAY_MS }),
    [#client_data{ time_since_last_ping = TimeSinceLastPing }] =
            ets:lookup(clients, Key),
    if TimeSinceLastPing >= ?CLIENT_TIMEOUT_MS ->
        io:format("Client ~p timed out (no response in ~p ms)~n", [Key,
                ?CLIENT_TIMEOUT_MS]),
        ets:delete(clients, Key);
       true -> ok
    end.

