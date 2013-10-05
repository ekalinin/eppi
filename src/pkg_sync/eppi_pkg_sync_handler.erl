-module(eppi_pkg_sync_handler).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link({save_file, FileName, Content}) ->
    %lager:info("* Link worker, 'save_file': ~p", [FileName]),
    gen_server:start_link(?SERVER, [save_file, FileName, Content], []);

start_link({send_to_node, FileName, {Server, Node}}) ->
    %lager:info("* Link worker, 'send_to_node': ~p@~p/~p",
    %    [FileName, Node, Server]),
    gen_server:start_link(?SERVER, [send_to_node, FileName, Server, Node], []);

start_link({get_from_pypi, FileName}) ->
    %lager:info("* Link worker, 'get_from_pypi': ~p", [FileName]),
    gen_server:start_link(?SERVER, [get_from_pypi, FileName], []);

start_link({get_from_node, FileName, {Server, Node}}) ->
    %lager:info("* Link worker, 'get_from_node': ~p@~p/~p", [FileName, Node, Server]),
    gen_server:start_link(?SERVER, [get_from_node, FileName, Server, Node], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([get_from_pypi, FileName, Parent]) ->
    {ok, #state{}};

init([save_file, FileName, Content]) ->
    %lager:info("-> Init 'save_file' worker for: ~p", [FileName]),
    gen_server:cast(self(), {save_file, FileName, Content}),
    {ok, #state{}};

init([send_to_node, FileName, Server, Node]) ->
    %lager:info("-> Init 'send_to_node' worker for: ~p@~p/~p",
    %    [FileName, Node, Server]),
    gen_server:cast(self(), {send_to_node, FileName, Server, Node}),
    {ok, #state{}};

init([get_from_node, FileName, Server, Node]) ->
    %lager:info("-> Init 'get_from_node' worker for: ~p@~p/~p",
    %    [FileName, Node, Server]),
    gen_server:cast(self(), {get_from_node, FileName, Server, Node}),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({get_from_node, FileName, Server, Node}, State) ->
    lager:info("-> Got 'get_from_node' for: ~p @ ~p",[FileName, Node]),
    gen_server:cast({Server, Node}, {give_me_file, FileName, node()}),
    {noreply, State};

handle_cast({send_to_node, FileName, Server, Node}, State) ->
    lager:info("* Sending ~p to ~p ...",[FileName, Node]),
    % find full file path
    {ok, PackagesDir} = eppi_utl:get_env(packages_dir),
    FullFileName = filename:absname(filename:join(PackagesDir, FileName)),
    lager:info(" * Read file: ~p", [FullFileName]),
    % read file & send it
    {ok, Content} = file:read_file(filename:join(PackagesDir, FileName)),
    gen_server:cast({Server, Node}, {save_file, FileName, Content}),
    lager:info("* File ~p was sent to ~p.",[FileName, Node]),
    % stop worker
    {stop, normal, State};

handle_cast({save_file, FileName, Content}, State) ->
    lager:info("* Receiving file: ~p", [FileName]),
    % find full file path
    {ok, PackagesDir} = eppi_utl:get_env(packages_dir),
    FullFileName = filename:absname(filename:join(PackagesDir, FileName)),
    lager:info(" * Write file: ~p", [FullFileName]),
    % write file
    ok = file:write_file(FullFileName, Content),
    lager:info("* File ~p received.", [FileName]),
    % stop worker
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    lager:info("* Terminating worker ..."),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
