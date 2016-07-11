-module(client).
-behavior(gen_statem).

-compile(export_all).

-define(PROCESS, process).
-define(CLIENT, ?MODULE).


%%===================================================================================================
%% API
%%===================================================================================================


start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

search(Terms, Types) ->
    Results = gen_statem:call(?CLIENT, {?CLIENT, {search, Types}, Terms}),
    io:format("~ts", [Results]),
    erlang:halt().

get(Terms, Type) ->
    gen_statem:call(?CLIENT, {?CLIENT, {get, Type}, Terms}),
    gen_statem:cast(?CLIENT, stop).

install(Terms, Type) ->
    gen_fsm:sync_send_event(?CLIENT, {?CLIENT, {install, Type}, Terms}),
    gen_fsm:cast(?CLIENT, stop).


%%===================================================================================================
%% Callback
%%===================================================================================================


init(Args) ->
    {state_functions, idle, Args}.

idle({call, From}, {?CLIENT, {search, Types}, Terms}, []) ->
    notify_server({?CLIENT, {search, Types}, Terms}),
    spawn_link(fun() -> ?PROCESS:search(Types) end),
    {next_state, wait, From};

idle({call, From}, {?CLIENT, {get, Type}, Terms}, []) ->
    notify_server({?CLIENT, {get, Type}, Terms}),
    spawn_link(fun() -> ?PROCESS:get(Type) end),
    {next_state, wait, From};

idle({call, From}, {?CLIENT, {install, Type}, Terms}, []) ->
    notify_server({?CLIENT, {install, Type}, Terms}),
    spawn_link(fun() -> ?PROCESS:install(Type) end),
    {next_state, wait, From}.

wait({call, ServerID}, {?PROCESS, {search, Types}, ready}, ClientID) ->
    {next_state, send, ClientID, {reply, ServerID, {?CLIENT, {search, Types}, ready}}};

wait({call, ServerID}, {?PROCESS, {get, Type}, ready}, ClientID) ->
    {next_state, send, ClientID, {reply, ServerID, {?CLIENT, {get, Type}, ready}}};

wait({call, ServerID}, {?PROCESS, {install, Type}, ready}, ClientID) ->
    {next_state, send, ClientID, {reply, ServerID, {?CLIENT, {get, Type}, ready}}}.

send(cast, {?PROCESS, deliver, Results}, From) ->
    gen_statem:reply(From, Results),
    {next_state, idle, []};

send(cast, {?PROCESS, get, ok}, From) ->
    gen_statem:reply(From, ok),
    {next_state, idle, []};

send(cast, {?PROCESS, get, fail}, From) ->
    % Download errors should be handled in this section
    gen_statem:reply(From, error),
    {next_state, idle, []};

send(cast, {?PROCESS, install, ok}, From) ->
    gen_statem:reply(From, ok),
    {next_state, idle, []};

send(cast, {?PROCESS, install, fail}, From) ->
    gen_statem:reply(From, error),
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
    gen_statem:call(?PROCESS, Status).

notify_server(Status) ->
    gen_statem:cast(?PROCESS, Status).
