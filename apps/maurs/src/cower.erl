-module(cower).
-behavior(gen_fsm).

%% API
-export(
    [ start_link/0
     ,search/1
     ,get/1 ]
).


%% Callback exports
-export(
    [ init/1
     ,idle/2
     ,search/2
     ,get/2
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

search(Types) ->
    case sync_notify_client({?SERVER, {search, Types}, ready}) of
        {?CLIENT, {search, Types}, ready} ->
            gen_fsm:send_event(?SERVER, {?SERVER, {search, Types}, ready})
    end.

get(Type) ->
    case sync_notify_client({?SERVER, {get, Type}, ready}) of
        {?CLIENT, {get, Type}, ready} ->
            gen_fsm:send_event(?SERVER, {?SERVER, {get, Type}, ready})
    end.


%%===================================================================================================
%% Callback
%%===================================================================================================


init(Args) ->
    {ok, idle, Args}.

idle({?CLIENT, {search, _Types}, Terms}, []) ->
    {next_state, search, Terms};

idle({?CLIENT, {get, _Type}, Terms}, []) ->
    {next_state, get, Terms}.

%% We should perform the search and notify the client of the results here
search({?SERVER, {search, Types}, ready}, Terms) ->
    {ReceiverPid, MRef} = spawn_monitor(fun() -> ?SERVER ! receive_search_results() end),
    register(receiver, ReceiverPid),
    spawn_link(fun() -> do_search(Terms, Types) end),
    receive
        {'DOWN', MRef, _, ReceiverPid, normal} ->
            demonitor(MRef, [flush]);
        {'DOWN', MRef, _, ReceiverPid, _Errors} ->
            io:format("error~n"),
            erlang:halt()
    end,
    receive
        Results -> notify_client({?SERVER, deliver, Results})
    end,
    {next_state, idle, []}.

%% We should perform the retrieval of the packages and notification of final status to client here
get({?SERVER, {get, aur}, ready}, Terms) ->
    case decode_aur_get(os:cmd(io_lib:format("cower -d ~s", [Terms]))) of
        error -> notify_client({?SERVER, get, fail});
        ok -> notify_client({?SERVER, get, ok})
    end,
    {next_state, idle, []};

get({?SERVER, {get, pacman}, ready}, Terms) ->
    case decode_pacman_get(os:cmd(io_lib:format("pacman -S ~s", [Terms]))) of
        error -> notify_client({?SERVER, get, fail});
        ok -> notify_client({?SERVER, get, fail})
    end,
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


%%===================================================================================================
%% Internal Functions
%%===================================================================================================

notify_client(Status) ->
    gen_fsm:send_event(?CLIENT, Status).

sync_notify_client(Status) ->
    gen_fsm:sync_send_event(?CLIENT, Status).

receive_search_results() ->
    receive
        {done, Result} -> [Result];
        {ok, Result} -> [Result|receive_search_results()]
    end.

do_search(Terms, [pacman_search]) ->
    Result = fun() -> os:cmd(io_lib:format("pacman -Ss ~s", [Terms])) end,
    spawn_link(fun() -> receiver ! {done, Result()} end);

do_search(Terms, [aur_search]) ->
    Result = fun() -> os:cmd(io_lib:format("cower -s ~s", [Terms])) end,
    spawn_link(fun() -> receiver ! {done, Result()} end);

do_search(Terms, [Type|Rest]) when Rest =/= [] ->
    case Type of
        pacman_search -> Result = fun() -> os:cmd(io_lib:format("pacman -Ss ~s", [Terms])) end;
        aur_search -> Result = fun() -> os:cmd(io_lib:format("cower -s ~s", [Terms])) end
    end,
    spawn_link(fun() -> receiver ! {ok, Result()} end),
    do_search(Terms, Rest).

decode_aur_get([101,114,114,111,114,58|_]) -> error;
decode_aur_get(_) -> ok.

decode_pacman_get([101,114,114,111,114,58|_]) -> error;
decode_pacman_get(_) -> ok.
