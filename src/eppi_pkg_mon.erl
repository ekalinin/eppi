-module(eppi_pkg_mon).

-behaviour(gen_server).

%% API
-export([
         get_files/0,
         get_files/1,
         get_packages/0,
         check_new/0,
         start_link/0
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
        % Files list
        files = [],
        % Package list
        packages = [],
        % Files monitor check interval
        check_period = 0
    }).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Returns list of files
get_files() ->
    gen_server:call(?SERVER, {get_files}).

%% @doc Returns list of packages for certain package
get_files(Package) ->
    lists:sort([F || F <- get_files(), get_package_for_file(F) == Package]).

%% @doc Returns list of packages
get_packages() ->
    gen_server:call(?SERVER, {get_packages}).

check_new() ->
    gen_server:call(?SERVER, {check_new}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    gen_server:cast(?SERVER, {start_server}),
    {ok, #state{}}.


handle_call({get_files}, _From, State) ->
    Reply = State#state.files,
    {reply, Reply, State};

handle_call({get_packages}, _From, State) ->
    Reply = State#state.packages,
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({start_server}, State) ->
    {ok, CheckPeriod} = eppi_utl:get_env(new_packages_check_period),
    {ok, PackagesDir} = eppi_utl:get_env(packages_dir),
    lager:info("+ Starting eppi:mon server, directory: ~p, period: ~p [secs]",
                                        [PackagesDir, CheckPeriod/1000]),
    case filelib:is_dir(PackagesDir) of
        true ->
            {ok, Files, Packages} = get_local_files_and_packages(PackagesDir),
            ok = eppi_cluster:notify_i_have(Files),
            ok = eppi_cluster:notify_what_is_yours(),
            erlang:send_after(CheckPeriod, self(), {check_new}),
            {noreply, State#state{ files = Files, packages = Packages,
                check_period = CheckPeriod}};
        false ->
            lager:debug("+ Directory doesn't exists ..."),
            ok = filelib:ensure_dir(PackagesDir),
            ok = file:make_dir(PackagesDir),
            lager:info("+ Directory created."),
            ok = eppi_cluster:notify_what_is_yours(),
            erlang:send_after(CheckPeriod, self(), {check_new}),
            {noreply, State#state{files = [], packages = [],
                check_period = CheckPeriod}}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Check new files (periodic task)
handle_info({check_new}, State) ->
    lager:debug("+ Checking new files ..."),
    {ok, PackagesDir} = eppi_utl:get_env(packages_dir),
    {ok, CurrFiles, CurrPackage} = get_local_files_and_packages(PackagesDir),
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
    erlang:send_after(State#state.check_period, self(), {check_new}),
    {noreply, State#state{files = CurrFiles, packages = CurrPackage}};

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

get_package_for_file(File) ->
    lists:nth(1, re:split(File, "-\\d+", [{return, list}])).

%% @doc Converts list of files to a list of packages.
get_packages_for_files(Files) ->
    % remove version from each file
    Packages = [get_package_for_file(F) || F <- Files],
    % remove dublicates & sort list
    lists:sort(sets:to_list(sets:from_list(Packages))).

%% @doc Returns turple {ok, Files, Packages}.
get_local_files_and_packages(Dir) ->
    Files = get_local_files(Dir),
    Packages = get_packages_for_files(Files),
    {ok, Files, Packages}.
