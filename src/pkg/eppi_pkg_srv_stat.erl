-module(eppi_pkg_srv_stat).

-behaviour(gen_server).

%% API
-export([
         start_link/0
         % get_packages/0,
         % get_packages/1,
         % get_files/0,
         % get_files/1,
         %
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
        % Package list
        packages = [],
        % Files lisy
        files = []
    }).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get_packages() ->
    get_packages(nodes()).
get_packages(Node) ->
    gen_server:call({?SERVER, Node}, {get_packages}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, PackagesDir} = application:get_env(eppi, packages_dir),
    % init server
    ok = gen_server:cast(?SERVER, {start_server, PackagesDir}),
    % init state
    {ok, #state{}}.

handle_call({get_packages}, _From, State) ->
    {reply, State#state.packages, State};

handle_call({get_files}, _From, State) ->
    {reply, State#state.files, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({start_server, PackagesDir}, State) ->
    case filelib:is_dir(PackagesDir) of
        true ->
            % Get packages — each directory name
            Packages = get_all_directories(PackagesDir),
            % Get files
            Files = get_all_files(PackagesDir),
            % return stats 
            {noreply, State#state{packages = Packages, files = Files}};
        false ->
            % some log
            lager:info("Directory doesn't exists: ~s", [PackagesDir]),
            ok = filelib:ensure_dir(PackagesDir),
            ok = file:make_dir(PackagesDir),
            lager:info("Directory created: ~s", [PackagesDir]),
            % return empty plugins list
            {noreply, State#state{packages = [], files = []}}
    end;


handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_all_files(Dir) ->
    FindFiles = fun(F, Acc) -> [F | Acc] end,
    filelib:fold_files(Dir, ".*", false, FindFiles, []).

get_all_directories(Path) ->
    lists:filter(
        fun(X) -> filelib:is_dir(X) end,
        filelib:wildcard(Path ++ "/*")
    ).
