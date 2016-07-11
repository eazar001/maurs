-module(process).
-behavior(gen_statem).

-compile(export_all).

-define(CLIENT, client).
-define(PROCESS, ?MODULE).


%%===================================================================================================
%% API
%%===================================================================================================


start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

search(Types) ->
    {?CLIENT, {search, Types}, ready} = sync_notify_client({?PROCESS, {search, Types}, ready}),
    gen_statem:cast(?PROCESS, {?PROCESS, {search, Types}, ready}).

get(Type) ->
    {?CLIENT, {get, Type}, ready} = sync_notify_client({?PROCESS, {get, Type}, ready}),
    gen_statem:cast(?PROCESS, {?PROCESS, {get, Type}, ready}).

install(Type) ->
    {?CLIENT, {get,Type}, ready} = sync_notify_client({?PROCESS, {install, Type}, ready}),
    gen_statem:cast(?PROCESS, {?PROCESS, {install, Type}, ready}).


%%===================================================================================================
%% Callback
%%===================================================================================================


init(Args) ->
    {state_functions, idle, Args}.

idle(cast, {?CLIENT, {search, _Types}, Terms}, []) ->
    {next_state, search, Terms};

idle(cast, {?CLIENT, {get, _Type}, Terms}, []) ->
    {next_state, get, Terms}.

%% We should perform the search and notify the client of the results here
search(cast, {?PROCESS, {search, Types}, ready}, Terms) ->
    collect_results_and_notify_client({start, [], []}, Terms, Types),
    {next_state, idle, []}.

%% We should perform the retrieval of the packages and notification of final status to client here
get(cast, {?PROCESS, {get, aur}, ready}, Terms) ->
    case decode_aur_get(os:cmd(io_lib:format("cower -d ~s", [Terms]))) of
        error -> notify_client({?PROCESS, get, fail});
        ok -> notify_client({?PROCESS, get, ok})
    end,
    {next_state, idle, []};

get(cast, {?PROCESS, {get, pacman}, ready}, Terms) ->
    case decode_pacman_get(os:cmd(io_lib:format("pacman -S ~s", [Terms]))) of
        error -> notify_client({?PROCESS, get, fail});
        ok -> notify_client({?PROCESS, get, ok})
    end,
    {next_state, idle, []}.

install(cast, {?PROCESS, {install, aur}, ready}, Package) ->
    do_install(Package),
    notify_client({?PROCESS, install, ok}),
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

notify_client(Status) ->
    gen_statem:cast(?CLIENT, Status).

sync_notify_client(Status) ->
    gen_statem:call(?CLIENT, Status).

receive_search_results(Count) when Count >= 0 ->
    register(receiver, self()),
    Results = receive_search_results_loop(Count),
    Sorted_Results = [Content || {_, Content} <- lists:sort(fun sort_by_type/2, Results)],
    {ok, Sorted_Results}.

receive_search_results_loop(0) ->
    receive
        {Type, Result} -> [{Type, Result}]
    end;

receive_search_results_loop(Count) when Count > 0 ->
    receive
        {Type, Result} -> [{Type, Result}|receive_search_results_loop(Count-1)]
    end.

do_search(Terms, [pacman_search]) ->
    Result = fun() -> os:cmd(io_lib:format("pacman -Ss ~s", [Terms])) end,
    spawn_link(fun() -> receiver ! {pacman_search, Result()} end);

do_search(Terms, [aur_search]) ->
    Result = fun() -> os:cmd(io_lib:format("cower -s ~s", [Terms])) end,
    spawn_link(fun() -> receiver ! {aur_search, Result()} end);

do_search(Terms, [Type|Rest]) when Rest =/= [] ->
    case Type of
        pacman_search -> Result = fun() -> os:cmd(io_lib:format("pacman -Ss ~s", [Terms])) end;
        aur_search -> Result = fun() -> os:cmd(io_lib:format("cower -s ~s", [Terms])) end
    end,
    spawn_link(fun() -> receiver ! {Type, Result()} end),
    do_search(Terms, Rest).

install_packages(Packages) ->
    lists:foreach(fun do_install/1, Packages).

do_install(Package) ->
    Cmd = io_lib:format("makepkg -si ~s", [Package]),
    os:cmd(Cmd).

collect_results_and_notify_client({Phase, Rtag, Stag}, Terms, Types) ->
    case Phase of
        start ->
            Len = length(Types) - 1,
            ReceiverPid = spawn_link(fun() -> ?PROCESS ! receive_search_results(Len) end),
            SearcherPid = spawn_link(fun() -> do_search(Terms, Types) end);

        continue ->
            {ReceiverPid, SearcherPid} = {Rtag, Stag}
    end,
    receive

        {ok, Results} ->
            notify_client({?PROCESS, deliver, Results}),
            {Rtag0, Stag0} = {ReceiverPid, SearcherPid},
            collect_results_and_notify_client({continue, Rtag0, Stag0}, Terms, Types)

    after 5000 ->
        exit(timeout)
    end.

valid_aur_packages(Packages) ->
    Cmd = io_lib:format("cower -i ~s", [Packages]),
    lists:all(fun(P) -> decode_aur_info(os:cmd(Cmd)) == ok end, [Packages]).

sort_by_type({pacman_search, _}, {aur_search, _}) -> true;
sort_by_type({aur_search, _}, {pacman_search, _}) -> false.

decode_aur_info([101,114,114,111,114,58|_]) -> error;
decode_aur_info(_) -> ok.

decode_aur_get([101,114,114,111,114,58|_]) -> error;
decode_aur_get(_) -> ok.

decode_pacman_get([101,114,114,111,114,58|_]) -> error;
decode_pacman_get(_) -> ok.
