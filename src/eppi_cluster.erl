-module(eppi_cluster).

-behaviour(gen_server).

%% API
-export([
         start_link/0,
         notify_i_have/2,
         notify_i_have/1,
         notify_what_is_yours/1,
         notify_what_is_yours/0,
         connect/1
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

connect(Node) ->
    pong = net_adm:ping(Node),
    lager:debug("* Broadcasting: new-node ..."),
    lists:foreach(
        fun(TargetNode) ->
            lager:debug("* Sending `new-node` to ~p ...", [TargetNode]),
            gen_server:cast({?SERVER, TargetNode}, {new_node, node()})
        end,
        nodes()),
    ok.


notify_i_have(Files, Node) ->
    lager:debug("* Sending `i-have` files: ~p to ~p ...", [Files, Node]),
    gen_server:cast({?SERVER, Node}, {i_have, node(), Files}).

notify_i_have(Files) ->
    lager:debug("* Broadcasting: `i-have` ~p to the cluster ...", [Files]),
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
    lager:info("* Starting eppi:stat server ..."),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({new_node, ReplyTo}, State) ->
    lager:info("* New node detected ~p", [ReplyTo]),
    notify_i_have(eppi_pkg_mon:get_files(), ReplyTo),
    notify_what_is_yours(ReplyTo),
    {noreply, State};

handle_cast({i_have, OwnerNode, Files}, State) ->
    lager:debug("* ~p has: ~p files", [OwnerNode, Files]),
    lists:map(
        fun (File) ->
            lager:debug("* Check existence ~p from ~p ...", [File, OwnerNode]),
            case lists:member(File, eppi_pkg_mon:get_files()) of
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
    notify_i_have(eppi_pkg_mon:get_files(), ReplyTo),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
