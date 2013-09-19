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

    % Root supervisor childrens
    Childrens = [

        % start package server
        {eppi_pkg_sup,
            {eppi_pkg_sup, start_link, []},
             permanent, brutal_kill, supervisor, []
        }%,

        % start http api
        %{eppi_http_sup,
        %    {eppi_http_sup, start_link, []},
        %    permanent, brutal_kill, supervisor, []
        %},

    ],

    {ok, { {one_for_one, 1, 60}, Childrens} }.
