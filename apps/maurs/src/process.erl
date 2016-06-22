-module(process).
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
-define(PROCESS, ?MODULE).


%%===================================================================================================
%% API
%%===================================================================================================


start_link() ->
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

search(Types) ->
    case sync_notify_client({?PROCESS, {search, Types}, ready}) of
        {?CLIENT, {search, Types}, ready} ->
            gen_fsm:send_event(?PROCESS, {?PROCESS, {search, Types}, ready})
    end.

get(Type) ->
    case sync_notify_client({?PROCESS, {get, Type}, ready}) of
        {?CLIENT, {get, Type}, ready} ->
            gen_fsm:send_event(?PROCESS, {?PROCESS, {get, Type}, ready})
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
search({?PROCESS, {search, Types}, ready}, Terms) ->
    collect_results({start, [], []}, Terms, Types),
    {next_state, idle, []}.

%% We should perform the retrieval of the packages and notification of final status to client here
get({?PROCESS, {get, aur}, ready}, Terms) ->
    case decode_aur_get(os:cmd(io_lib:format("cower -d ~s", [Terms]))) of
        error -> notify_client({?PROCESS, get, fail});
        ok -> notify_client({?PROCESS, get, ok})
    end,
    {next_state, idle, []};

get({?PROCESS, {get, pacman}, ready}, Terms) ->
    case decode_pacman_get(os:cmd(io_lib:format("pacman -S ~s", [Terms]))) of
        error -> notify_client({?PROCESS, get, fail});
        ok -> notify_client({?PROCESS, get, fail})
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

terminate(halt, _StateName, _StateData) ->
    erlang:halt();

terminate(_Reason, _StateName, _StateData) ->
    case whereis(receiver) of
        undefined -> ok;
        _ -> unregister(receiver)
    end.


%%===================================================================================================
%% Internal Functions
%%===================================================================================================

notify_client(Status) ->
    gen_fsm:send_event(?CLIENT, Status).

sync_notify_client(Status) ->
    gen_fsm:sync_send_event(?CLIENT, Status).

receive_search_results(Count) when Count >= 0 ->
    case whereis(receiver) of
        undefined -> register(receiver, self());
        _ ->
            unregister(receiver),
            register(receiver, self())
    end,
    {ok, receive_search_results_loop(Count)}.

receive_search_results_loop(0) ->
    receive
        {ok, Result} -> [Result]
    end;

receive_search_results_loop(Count) when Count > 0 ->
    receive
        {ok, Result} -> [Result|receive_search_results_loop(Count-1)]
    end.

do_search(Terms, [pacman_search]) ->
    Result = fun() -> os:cmd(io_lib:format("pacman -Ss ~s", [Terms])) end,
    spawn_link(fun() -> receiver ! {ok, Result()} end);

do_search(Terms, [aur_search]) ->
    Result = fun() -> os:cmd(io_lib:format("cower -s ~s", [Terms])) end,
    spawn_link(fun() -> receiver ! {ok, Result()} end);

do_search(Terms, [Type|Rest]) when Rest =/= [] ->
    case Type of
        pacman_search -> Result = fun() -> os:cmd(io_lib:format("pacman -Ss ~s", [Terms])) end;
        aur_search -> Result = fun() -> os:cmd(io_lib:format("cower -s ~s", [Terms])) end
    end,
    spawn_link(fun() -> receiver ! {ok, Result()} end),
    do_search(Terms, Rest).

collect_results({Phase, Rtag, Stag}, Terms, Types) ->
    case Phase of
        start ->
            Len = length(Types) - 1,
            {ReceiverPid, Rmref} = spawn_monitor(fun() -> ?PROCESS ! receive_search_results(Len) end),
            {SearcherPid, Smref} = spawn_monitor(fun() -> do_search(Terms, Types) end);

        continue ->
            {{ReceiverPid, Rmref}, {SearcherPid, Smref}} = {Rtag, Stag}
    end,
    receive

        {ok, Results} ->
            notify_client({?PROCESS, deliver, Results}),
            demonitor(Rmref, [flush]),
            demonitor(Smref, [flush]),
            {Rtag0, Stag0} = {{ReceiverPid, Rmref}, {SearcherPid, Smref}},
            collect_results({continue, Rtag0, Stag0}, Terms, Types);

        {'DOWN', Rmref, _, ReceiverPid, Reason} ->
            exit(Reason);

        {'DOWN', Smref, _, SearcherPid, normal} ->
            {Rtag0, Stag0} = {{ReceiverPid, Rmref}, {SearcherPid, Smref}},
            collect_results({continue, Rtag0, Stag0}, Terms, Types);

        {'DOWN', Smref, _, SearcherPid, Reason} ->
            exit(Reason)

    after 5000 ->
        lists:foreach(fun(Mon) -> demonitor(Mon, [flush]) end, [Rmref, Smref]),
        exit(timeout)
    end.

decode_aur_get([101,114,114,111,114,58|_]) -> error;
decode_aur_get(_) -> ok.

decode_pacman_get([101,114,114,111,114,58|_]) -> error;
decode_pacman_get(_) -> ok.
