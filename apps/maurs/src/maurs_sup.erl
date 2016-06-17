%%%-------------------------------------------------------------------
%% @doc maurs top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(maurs_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(CLIENT, client).
-define(SERVER, cower).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    SupFlags = #{
        strategy => one_for_all,
        intensity => 1,
        period => 5
    },
    ChildSpecs = [
        #{ id => ?CLIENT,
           start => {?CLIENT, start_link, []},
           restart => transient,
           shutdown => brutal_kill,
           type => worker,
           modules => [?CLIENT]
         },
        #{ id => ?SERVER,
           start => {?SERVER, start_link, []},
           restart => transient,
           shutdown => brutal_kill,
           type => worker,
           modules => [?SERVER]
         }
    ],
    {ok, { SupFlags, ChildSpecs} }.
