-module(client).
-behavior(gen_fsm).

%% API
-export(
    [ start_link/0
     ,search/1
     ,notify_server/1
     ,sync_notify_server/1 ]
).

%% Callback exports
-export(
    [ init/1
     ,idle/3
     ,wait/3
     ,send/2
     ,handle_event/3
     ,handle_sync_event/4
     ,handle_info/3
     ,code_change/4
     ,terminate/3 ]
).

-define(SERVER, cower).
-define(CLIENT, ?MODULE).


%%===================================================================================================
%% API
%%===================================================================================================


start_link() ->
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

search(Terms) ->
    Results = gen_fsm:sync_send_event(?MODULE, {?CLIENT, search, Terms}),
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

idle({?CLIENT, search, Terms}, From, []) ->
    notify_server({?CLIENT, ready, Terms}),
    spawn_link(fun() -> ?SERVER:search() end),
    {next_state, wait, From}.

wait({?SERVER, search, ready}, _ServerID, ClientID) ->
    {reply, {?CLIENT, search, ready}, send, ClientID}.

send({?SERVER, deliver, Results}, From) ->
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
