-module(eppi_http).

-behaviour(gen_server).

%% API
-export([
         start_link/0
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(EPPI_ACCEPTORS,  100).
-define(EPPI_STATIC,  "static/").

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    % start server
    ok = gen_server:cast(self(), start_serve),
    % init internal state
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%%
%% @doc Starts cowboy & set up routes for it
%%
handle_cast(start_serve, State) ->
    % Get http port
    {ok, HttpPort} = application:get_env(eppi, http_port),
    % Cowboy dispatch
    Dispatch = dispatch_rules(),
    % start serving
    {ok, _} = cowboy:start_http(http_listener, ?EPPI_ACCEPTORS,
        [{port, HttpPort}],
        [{env, [{dispatch, Dispatch}]}]
    ),
    % return
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

static(Filetype) ->
    Url = lists:append(["/", Filetype, "/[...]"]),
    Path = list_to_binary(?EPPI_STATIC ++ Filetype),
    {Url, cowboy_static, [
            {directory, {priv_dir, eppi, [Path]}},
            {mimetypes, {fun mimetypes:path_to_mimes/2, default}}
    ]}.

dispatch_rules() ->
    cowboy_router:compile([
        {'_', [
            static("css"),
            static("js"),
            static("fonts"),
            static("img"),
            %{"/", index_handler, []},
            {"/", cowboy_static, [
                {directory, {priv_dir, eppi, [?EPPI_STATIC]}},
                {file, <<"index.html">>},
                {mimetypes, {fun mimetypes:path_to_mimes/2, default}}
            ]},
            {'_', notfound_handler, []}
        ]}
    ]).
