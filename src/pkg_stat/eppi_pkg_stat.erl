-module(eppi_pkg_stat).

-behaviour(gen_server).

%% API
-export([
         start_link/0,
         get_files/0,
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

connect(Node) ->
    pong = net_adm:ping(Node),
    lager:info("* Broadcasting: new-node ..."),
    lists:foreach(
        fun(TargetNode) ->
            lager:info("<-- sending `new-node` to ~p ...", [TargetNode]),
            gen_server:cast({?SERVER, TargetNode},
                            {new_node, node()})
        end,
        nodes()),
    ok.

refresh_files() ->
    gen_server:cast(?SERVER, {refresh_files}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    % init server
    ok = gen_server:cast(?SERVER, {start_server}),
    % init state
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = State#state.files,
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({refresh_files}, State) ->
    lager:info("-> Got 'refresh_files' ..."),
    {ok, PackagesDir} = eppi_utl:get_env(packages_dir),
    Files = get_local_files(PackagesDir),
    lager:info("--> Found files: ~p", [length(Files)]),
    {noreply, State#state{files = Files}};

handle_cast({start_server}, State) ->
    {ok, PackagesDir} = eppi_utl:get_env(packages_dir),
    lager:info("-> Got 'start_server', for directory: ~s", [PackagesDir]),
    case filelib:is_dir(PackagesDir) of
        true ->
            Files = get_local_files(PackagesDir),
            lager:info("--> Found files: ~p", [length(Files)]),
            ok = notify_i_have(Files),
            ok = notify_what_is_yours(),
            % return
            {noreply, State#state{files = Files}};
        false ->
            lager:info("--> Directory doesn't exists."),
            ok = filelib:ensure_dir(PackagesDir),
            ok = file:make_dir(PackagesDir),
            lager:info("--> Directory created."),
            ok = notify_what_is_yours(),
            % return
            {noreply, State#state{files = []}}
    end;

handle_cast({new_node, ReplyTo}, State) ->
    lager:info("-> Got 'new_node' from ~p", [ReplyTo]),
    notify_i_have(State#state.files, ReplyTo),
    notify_what_is_yours(ReplyTo),
    {noreply, State};

handle_cast({i_have_new, ReplyTo, FileName}, State) ->
    lager:info("-> Got 'i_have_new' [~p] from ~p", [FileName, ReplyTo]),
    {noreply, State};

handle_cast({i_have, OwnerNode, Files}, State) ->
    lager:info("-> Got 'i_have' [~p] from ~p", [Files, OwnerNode]),
    lists:map(
        fun (File) ->
            case lists:member(File, State#state.files) of
                false -> eppi_pkg_sync:get_from_node(File, OwnerNode);
                true ->  ok
            end
        end,
        Files),
    {noreply, State};

handle_cast({what_is_yours, ReplyTo}, State) ->
    lager:info("-> Got 'what_is_yours` from ~p", [ReplyTo]),
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


%%
%% Notify utils
%%
notify_new(FileName, Node) ->
    lager:info("<-- Sending `i-have-new` to ~p ...", [Node]),
    gen_server:cast({?SERVER, Node}, {i_have_new, node(), FileName}).

notify_new(FileName) ->
    lager:info("<- Broadcasting: i-have-new ..."),
    lists:foreach(
        fun(Node) -> notify_new(FileName, Node) end,
        nodes()),
    ok.


notify_i_have(Files, Node) ->
    lager:info("<-- Sending `i-have` to ~p ...", [Node]),
    gen_server:cast({?SERVER, Node}, {i_have, node(), Files}).

notify_i_have(Files) ->
    lager:info("<- Broadcasting: i-have ..."),
    lists:foreach(
        fun(Node) -> notify_i_have(Files, Node) end,
        nodes()),
    ok.

notify_what_is_yours(Node) ->
    lager:info("<-- Sending `what-is-yours` to ~p ...", [Node]),
    gen_server:cast({?SERVER, Node}, {what_is_yours, node()}).

notify_what_is_yours() ->
    lager:info("<- Broadcasting: what-is-yours ..."),
    lists:foreach(
        fun(Node) -> notify_what_is_yours(Node) end,
        nodes()),
    ok.
