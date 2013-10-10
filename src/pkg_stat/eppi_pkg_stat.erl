-module(eppi_pkg_stat).

-behaviour(gen_server).

%% API
-export([
         start_link/0,
         notify_i_have/2,
         notify_i_have/1,
         notify_what_is_yours/1,
         notify_what_is_yours/0,
         get_files/0,
         get_files/1,
         refresh_files/0,
         connect/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
        % Files list
        files = []
    }).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get_files() ->
    gen_server:call(?SERVER, {get_files}).
get_files(true) ->
    {ok, PackagesDir} = eppi_utl:get_env(packages_dir),
    Files = get_local_files(PackagesDir),
    gen_server:cast(?SERVER, {refresh_files, Files}),
    Files.

connect(Node) ->
    pong = net_adm:ping(Node),
    lager:debug("* Broadcasting: new-node ..."),
    lists:foreach(
        fun(TargetNode) ->
            lager:debug(" sending `new-node` to ~p ...", [TargetNode]),
            gen_server:cast({?SERVER, TargetNode},
                            {new_node, node()})
        end,
        nodes()),
    ok.

refresh_files() ->
    gen_server:cast(?SERVER, {refresh_files}).


notify_i_have(Files, Node) ->
    lager:debug("* Sending `i-have` files: ~p to ~p ...", [Files, Node]),
    gen_server:cast({?SERVER, Node}, {i_have, node(), Files}).

notify_i_have(Files) ->
    lager:debug("* Broadcasting: `i-have` files: ~p to the cluster ...", [Files]),
    lists:foreach(
        fun(Node) -> notify_i_have(Files, Node) end,
        nodes()),
    ok.

notify_what_is_yours(Node) ->
    lager:debug("* Sending `what-is-yours` files to ~p ...", [Node]),
    gen_server:cast({?SERVER, Node}, {what_is_yours, node()}).

notify_what_is_yours() ->
    lager:debug("* Broadcasting: `what-is-yours` files to the cluster ..."),
    lists:foreach(
        fun(Node) -> notify_what_is_yours(Node) end,
        nodes()),
    ok.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    % init server
    ok = gen_server:cast(?SERVER, {start_server}),
    % init state
    {ok, #state{}}.

handle_call({get_files}, _From, State) ->
    Reply = State#state.files,
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({refresh_files, Files}, State) ->
    lager:debug("* Refresh filelist cache, new list: ~p", [Files]),
    {noreply, State#state{files = Files}};

handle_cast({refresh_files}, State) ->
    lager:info("-> Got 'refresh_files/1' ..."),
    Files = get_files(true),
    lager:info("-> Found files: ~p", [length(Files)]),
    {noreply, State#state{files = Files}};

handle_cast({start_server}, State) ->
    {ok, PackagesDir} = eppi_utl:get_env(packages_dir),
    lager:info("* Starting eppi:stat server, directory: ~s", [PackagesDir]),
    case filelib:is_dir(PackagesDir) of
        true ->
            Files = get_local_files(PackagesDir),
            lager:info("* Found files: ~p", [length(Files)]),
            ok = notify_i_have(Files),
            ok = notify_what_is_yours(),
            % return
            {noreply, State#state{files = Files}};
        false ->
            ok = filelib:ensure_dir(PackagesDir),
            ok = file:make_dir(PackagesDir),
            lager:info("* Directory created."),
            ok = notify_what_is_yours(),
            % return
            {noreply, State#state{files = []}}
    end;

handle_cast({new_node, ReplyTo}, State) ->
    lager:info("* New node detected ~p", [ReplyTo]),
    notify_i_have(State#state.files, ReplyTo),
    notify_what_is_yours(ReplyTo),
    {noreply, State};

handle_cast({i_have, OwnerNode, Files}, State) ->
    lager:debug("* ~p has: ~p files", [OwnerNode, Files]),
    lists:map(
        fun (File) ->
            lager:debug("* Check existence ~p from ~p ...", [File, OwnerNode]),
            case lists:member(File, State#state.files) of
                false ->
                    lager:info("* Sync ~p from ~p ...", [File, OwnerNode]),
                    eppi_pkg_sync:get_from_node(File, OwnerNode);
                true -> 
                    lager:debug("* File already exists: ~p", [File]),
                    ok
            end
        end,
        Files),
    {noreply, State};

handle_cast({what_is_yours, ReplyTo}, State) ->
    notify_i_have(State#state.files, ReplyTo),
    {noreply, State};

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


