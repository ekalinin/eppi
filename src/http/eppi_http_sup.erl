-module(eppi_http_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% ===================================================================
%% API
%% ===================================================================
%%
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child() ->
    % run http server
    supervisor:start_child(?SERVER, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init([]) ->
    % Check ranch and cowboy
    case whereis(ranch) of
        undefined ->
            % start ranch application
            application:start(ranch),
            % start cowboy application
            application:start(cowboy);
        _ ->
            ok
    end,

    % http server child process
    ChildSpec = [
        {eppi_http,
            {eppi_http, start_link, []},
             permanent, 2000, worker, []
        }
    ],

    % init
    {ok,{{one_for_one, 10, 60}, ChildSpec}}.
