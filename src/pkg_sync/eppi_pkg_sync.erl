-module(eppi_pkg_sync).

-behaviour(gen_server).

%% API
-export([
         start_link/0,
         get_from_pypi/1,
         get_from_node/2
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Get file from pypi
get_from_pypi(FileName) ->
    eppi_pkg_sync_sup:worker({get_from_pypi, FileName}).

%% @doc Get file from eppi's Node
get_from_node(FileName, Node) ->
    gen_server:cast({?SERVER, Node}, {send_me_file, FileName, node()}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% @doc Some Node asks current Node to send it a file
handle_cast({send_me_file, FileName, Node}, State) ->
    lager:debug("- Got `send_me_file` for: ~p from: ~p",[FileName, Node]),
    eppi_pkg_sync_sup:worker({send_to_node, FileName, {?SERVER, Node}}),
    {noreply, State};

%% @doc Some Node send a file to a current node
handle_cast({save_file, FileName, Content}, State) ->
    lager:debug("- Got `save_file` for: ~p",[FileName]),
    eppi_pkg_sync_sup:worker({save_file, FileName, Content}),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
