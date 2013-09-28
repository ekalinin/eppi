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

start_link({from_pypi, FileName}) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE,
        [from_pypi, FileName], []);
start_link({from_node, FileName, Node}) ->
    lager:info(" + Start worker for sync ~p from ~p ...", [FileName, Node]),
    gen_server:start_link(?SERVER, [from_node, FileName, Node], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([from_pypi, FileName]) ->
    {ok, #state{}};

init([from_node, FileName, Node]) ->
    lager:info(" ++ Init worker for sync ~p from ~p ...", [FileName, Node]),
    gen_server:cast(self(), {from_node, FileName, Node}),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({from_node, FileName, Node}, State) ->
    lager:info(" +++ Start syncing ~p from ~p ...", [FileName, Node]),
    gen_server:cast({?SERVER, Node}, {give_me_file, FileName, node()}),
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
