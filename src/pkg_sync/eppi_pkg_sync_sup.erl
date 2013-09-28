-module(eppi_pkg_sync_sup).

-behaviour(supervisor).

%% API
-export([
         start_link/0,
         sync/1
        ]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

sync(Args) ->
    supervisor:start_child(?SERVER, [Args]).

%%%===================================================================
%%% supervisor callbacks
%%%===================================================================

init([]) ->
    Children = [
        {eppi_pkg_sync_handler,
            {eppi_pkg_sync_handler, start_link, []},
            temporary, brutal_kill, worker, []}
    ],
    RestartStrategy = {simple_one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.
