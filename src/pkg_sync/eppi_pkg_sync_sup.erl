-module(eppi_pkg_sync_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    Children = [
        {eppi_pkg_sync,
            {eppi_pkg_sync, start_link, []},
            temporary, brutal_kill, worker, [eppi_pkg_sync]}
    ],
    RestartStrategy = {simple_one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.
