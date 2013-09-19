-module(eppi_pkg_sup).

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

        % Stats server
        {eppi_pkg_srv_stat,
            {eppi_pkg_srv_stat, start_link, []},
            permanent, 2000, worker, []},

        % Sync server
        {eppi_pkg_srv_sync,
            {eppi_pkg_srv_sync, start_link, []},
            permanent, 2000, worker, []}
    ],

    RestartStrategy = {one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.
