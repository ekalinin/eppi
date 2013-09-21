-module(eppi_pkg_stat_sup).

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
        {eppi_pkg_stat,
            {eppi_pkg_stat, start_link, []},
            permanent, 2000, worker, [eppi_pkg_stat]}
    ],
    RestartStrategy = {one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.
