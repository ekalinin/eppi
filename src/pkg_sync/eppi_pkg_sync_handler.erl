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

start_link(Args) ->
    gen_server:start_link(?SERVER, Args, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Args) ->
    gen_server:cast(self(), Args),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({get_from_node, FileName, {Server, Node}}, State) ->
    lager:info("-> Got 'get_from_node' for: ~p @ ~p",[FileName, Node]),
    gen_server:cast({Server, Node}, {give_me_file, FileName, node()}),
    {noreply, State};

handle_cast({send_to_node, FileName, {Server, Node}}, State) ->
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
