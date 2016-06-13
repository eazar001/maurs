-module(cower).
-behavior(gen_fsm).

%% API
-export(
    [ start/0
     ,search/0
     ,notify_client/1
     ,sync_notify_client/1 ]).


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


%%===================================================================================================
%% API
%%===================================================================================================


start() ->
    {ok, Pid} = gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []),
    {ok, Pid}.

search() ->
    case sync_notify_client(ready) of
        client_ready ->
            gen_fsm:send_event(?MODULE, ready);
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

idle({ready, Terms}, []) ->
    {next_state, search, Terms}.

%% We should perform the search and notify the client of the results here
search(ready, Terms) ->
    Cmd = io_lib:format("cower -s ~s~n", [Terms]),
    Results = os:cmd(Cmd),
    notify_client(Results),
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
