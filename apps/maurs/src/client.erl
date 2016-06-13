-module(client).
-behavior(gen_fsm).

%% API
-export(
    [ start/0
     ,search/1
     ,notify_server/1
     ,sync_notify_server/1 ]
).

%% Callback exports
-export(
    [ init/1
     ,idle/3
     ,wait/3
     ,send_results/2
     ,handle_event/3
     ,handle_sync_event/4
     ,handle_info/3
     ,code_change/4
     ,terminate/3 ]
).

-define(SERVER, cower).


%%===================================================================================================
%% API
%%===================================================================================================


start() ->
    {ok, Pid} = gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []),
    {ok, Pid}.

search(Terms) ->
    Results = gen_fsm:sync_send_event(?MODULE, {search, Terms}),
    io:format("~ts", [Results]).

sync_notify_server(Status) ->
    gen_fsm:sync_send_event(?SERVER, Status).

notify_server(Status) ->
    gen_fsm:send_event(?SERVER, Status).


%%===================================================================================================
%% Callback
%%===================================================================================================


init(Args) ->
    {ok, idle, Args}.

idle({search, Terms}, From, []) ->
    notify_server({ready, Terms}),
    spawn_link(fun() -> ?SERVER:search() end),
    {next_state, wait, From}.

wait(ready, _ServerID, ClientID) ->
    {reply, client_ready, send_results, ClientID}.

send_results(Results, From) ->
    gen_fsm:reply(From, Results),
    {next_state, idle, []}.

handle_event(stop, _StateName, _StateData) ->
    {stop, normal, []}.

handle_sync_event(stop, _From, _StateName, _StateData) ->
    {stop, normal, client_stop, []}.

handle_info(stop, _StateName, _StateData) ->
    {stop, normal, []}.

code_change(_OldVsn, _StateName, [], _Extra) ->
    {ok, idle, []}.

terminate(_Reason, _StateName, _StateData) -> ok.
