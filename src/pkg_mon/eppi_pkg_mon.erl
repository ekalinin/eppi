-module(eppi_pkg_mon).

-behaviour(gen_server).

%% API
-export([
         get_files/1,
         get_files/0,
         refresh_files/0,
         start_link/0
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
        % Files list
        files = [],
        % Files monitor check interval
        check_period = 0
    }).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get_files(true) ->
    {ok, PackagesDir} = eppi_utl:get_env(packages_dir),
    Files = get_local_files(PackagesDir),
    gen_server:cast(?SERVER, {refresh_files, Files}),
    Files;
get_files(_) ->
    get_files().
get_files() ->
    gen_server:call(?SERVER, {get_files}).

refresh_files() ->
    gen_server:cast(?SERVER, {refresh_files}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    gen_server:cast(?SERVER, {start_server}),
    {ok, #state{}}.

handle_call({get_files}, _From, State) ->
    Reply = State#state.files,
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({refresh_files, Files}, State) ->
    lager:info("+ Refresh filelist cache, new list: ~p", [Files]),
    {noreply, State#state{files = Files}};

handle_cast({refresh_files}, State) ->
    lager:info("+ Refreshing filelist cache ..."),
    Files = get_files(true),
    lager:info("+ Found files: ~p", [length(Files)]),
    {noreply, State#state{files = Files}};

handle_cast({start_server}, State) ->
    {ok, CheckPeriod} = eppi_utl:get_env(new_packages_check_period),
    {ok, PackagesDir} = eppi_utl:get_env(packages_dir),
    lager:info("+ Starting eppi:mon server, directory: ~p, period: ~p [secs]",
                                        [PackagesDir, CheckPeriod/1000]),
    case filelib:is_dir(PackagesDir) of
        true ->
            Files = get_files(true),
            ok = eppi_cluster:notify_i_have(Files),
            ok = eppi_cluster:notify_what_is_yours();
        false ->
            Files = [],
            lager:debug("+ Directory doesn't exists ..."),
            ok = filelib:ensure_dir(PackagesDir),
            ok = file:make_dir(PackagesDir),
            lager:info("+ Directory created."),
            ok = eppi_cluster:notify_what_is_yours()
    end,
    erlang:send_after(CheckPeriod, self(), {check_new_packages}),
    {noreply, State#state{files = Files, check_period = CheckPeriod}};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Check new files (periodic task)
handle_info({check_new_packages}, State) ->
    lager:debug("+ Checking new files ..."),
    CurrFiles = get_files(true),
    PrevFiles = State#state.files,
    % Try to get difference between files
    case CurrFiles -- PrevFiles of
        % No new files
        [] ->
            lager:debug("+ nothing new."),
            ok;
        % Got new files
        NewFiles ->
            lager:info("+ found files: ~p", [NewFiles]),
            eppi_cluster:notify_i_have(NewFiles)
    end,
    erlang:send_after(State#state.check_period, self(), {check_new_packages}),
    {noreply, State#state{files = CurrFiles}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%%
%%% Package — sub-directory
%%% Version — file in sub-directory
%%%

%% @doc Internal. For Accumulation files.
find_files_int(F, Acc) -> [F | Acc].

%% @doc Returns all files in sub-directories
get_local_files(Dir) ->
    Versions = filelib:fold_files(Dir, ".*", true,
        fun find_files_int/2, []),
    lists:map(fun filename:basename/1, Versions).

%% @doc Returnds all files in the package's directory
get_local_files(Dir, Package) ->
    Versions = filelib:fold_files(filename:join(Dir, Package),
        ".*", true, fun find_files_int/2, []),
    lists:map(fun filename:basename/1, Versions).
