-module(eppi_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init([]) ->
    Childrens = [
        % start sync handlers supervisor
        {eppi_pkg_sync_sup,
            {eppi_pkg_sync_sup, start_link, []},
             permanent, brutal_kill, supervisor, []
        },

        % start sync manager
        {eppi_pkg_sync,
            {eppi_pkg_sync, start_link, []},
             permanent, 2000, worker, []
        },

        % start package stats server
        {eppi_pkg_stat,
            {eppi_pkg_stat, start_link, []},
            permanent, 2000, worker, []
        },

        % start package monitoring server
        {eppi_pkg_mon,
            {eppi_pkg_mon, start_link, []},
            permanent, 2000, worker, []
        }

        % start http api
        %{eppi_http_sup,
        %    {eppi_http_sup, start_link, []},
        %    permanent, brutal_kill, supervisor, []
        %},
    ],

    {ok, { {one_for_one, 0, 1}, Childrens} }.
