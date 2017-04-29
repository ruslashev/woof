-module(woof_serv_main_loop).
-behaviour(gen_server).
-export([start_link/0]). % , new_client/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).
-include("woof_common.hrl").

start_link() ->
    gen_server:start_link({ local, woof_serv_main_loop }, ?MODULE, [], []).

% TODO
% new_client(ClientId) ->
%     gen_server:cast(woof_serv_main_loop, { new_client, ClientId }).

init([]) ->
    ets:new(clients, [set, { keypos, 2 }, public, named_table]),
    ets:new(players, [set, { keypos, 2 }, public, named_table]),
    Timer = erlang:send_after(1, self(), tick),
    { ok, Timer }.

handle_info(tick, Timer) ->
    erlang:cancel_timer(Timer),
    Players = ets:tab2list(players),
    if Players =/= [] ->
        UpdateMsg = woof_utils:update_msg(Players),
        send_update_to_players(Players, UpdateMsg);
       true -> ok
    end,
    NewTimer = erlang:send_after(?SERVER_UPDATE_SEND_DELAY_MS, self(), tick),
    { noreply, NewTimer };
handle_info({ new_client, ClientId }, Timer) ->
    case ets:lookup(players, ClientId) of
        [] -> ets:insert(players, #player{ client_id = ClientId });
        [_ExistingPlayer] -> ok
    end,
    io:format("newfriend~n"),
    io:format("firends: ~p~n", [ets:tab2list(players)]),
    { noreply, Timer };
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

send_update_to_players([], _UpdateMsg) ->
    ok;
send_update_to_players([#player{ client_id = ClientId } | TailPlayers],
                       UpdateMsg) ->
    io:format("Sending ~p to ~p~n", [UpdateMsg, ClientId]),
    % woof_utils:send(ClientId, UpdateMsg),
    send_update_to_players(TailPlayers, UpdateMsg).

