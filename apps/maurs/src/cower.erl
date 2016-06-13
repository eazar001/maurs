-module(cower).
-behavior(gen_fsm).

%% API
-export(
    [ start_link/0
     ,search/0
     ,notify_client/1
     ,sync_notify_client/1 ]
).


%% Callback exports
-export(
    [ init/1
     ,idle/2
     ,search/2
     ,handle_event/3
     ,handle_sync_event/4
     ,handle_info/3
     ,code_change/4
     ,terminate/3 ]
).

-define(CLIENT, client).
-define(SERVER, ?MODULE).


%%===================================================================================================
%% API
%%===================================================================================================


start_link() ->
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

search() ->
    case sync_notify_client({?SERVER, search, ready}) of
        {?CLIENT, search, ready} ->
            gen_fsm:send_event(?SERVER, {?SERVER, search, ready});
        _ -> fail
    end.

notify_client(Status) ->
    gen_fsm:send_event(?CLIENT, Status).

sync_notify_client(Status) ->
    gen_fsm:sync_send_event(?CLIENT, Status).


%%===================================================================================================
%% Callback
%%===================================================================================================


init(Args) ->
    {ok, idle, Args}.

idle({?CLIENT, ready, Terms}, []) ->
    {next_state, search, Terms}.

%% We should perform the search and notify the client of the results here
search({?SERVER, search, ready}, Terms) ->
    Cmd = io_lib:format("cower -s ~s", [Terms]),
    Results = os:cmd(Cmd),
    notify_client({?SERVER, deliver, Results}),
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
