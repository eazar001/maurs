-module(client).
-behavior(gen_fsm).

%% API
-export(
    [ start_link/0
     ,search/2
     ,get/2 ]
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

-define(PROCESS, process).
-define(CLIENT, ?MODULE).


%%===================================================================================================
%% API
%%===================================================================================================


start_link() ->
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

search(Terms, Types) ->
    Results = gen_fsm:sync_send_event(?CLIENT, {?CLIENT, {search, Types}, Terms}),
    io:format("~ts", [Results]),
    erlang:halt().

get(Terms, Type) ->
    gen_fsm:sync_send_event(?CLIENT, {?CLIENT, {get, Type}, Terms}),
    gen_fsm:send_all_state_event(?CLIENT, stop).


%%===================================================================================================
%% Callback
%%===================================================================================================


init(Args) ->
    {ok, idle, Args}.

idle({?CLIENT, {search, Types}, Terms}, From, []) ->
    notify_server({?CLIENT, {search, Types}, Terms}),
    spawn_link(fun() -> ?PROCESS:search(Types) end),
    {next_state, wait, From};

idle({?CLIENT, {get, Type}, Terms}, From, []) ->
    notify_server({?CLIENT, {get, Type}, Terms}),
    spawn_link(fun() -> ?PROCESS:get(Type) end),
    {next_state, wait, From}.

wait({?PROCESS, {search, Types}, ready}, _ServerID, ClientID) ->
    {reply, {?CLIENT, {search, Types}, ready}, send, ClientID};

wait({?PROCESS, {get, Type}, ready}, _ServerID, ClientID) ->
    {reply, {?CLIENT, {get, Type}, ready}, send, ClientID}.

send({?PROCESS, deliver, Results}, From) ->
    gen_fsm:reply(From, Results),
    {next_state, idle, []};

send({?PROCESS, get, ok}, From) ->
    gen_fsm:reply(From, ok),
    {next_state, idle, []};

send({?PROCESS, get, fail}, From) ->
    % Download errors should be handled in this section
    gen_fsm:reply(From, error),
    {next_state, idle, []}.

handle_event(stop, _StateName, _StateData) ->
    {stop, normal, []}.

handle_sync_event(stop, _From, _StateName, _StateData) ->
    {stop, normal, client_stop, []}.

handle_info(stop, _StateName, _StateData) ->
    {stop, normal, []}.

code_change(_OldVsn, _StateName, [], _Extra) ->
    {ok, idle, []}.

terminate(halt, _StateName, _StateData) ->
    erlang:halt();

terminate(_Reason, _StateName, _StateData) -> ok.


%%===================================================================================================
%% Internal Functions
%%===================================================================================================


sync_notify_server(Status) ->
    gen_fsm:sync_send_event(?PROCESS, Status).

notify_server(Status) ->
    gen_fsm:send_event(?PROCESS, Status).
