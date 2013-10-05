-module(eppi_pkg_mon).

-behaviour(gen_server).

%% API
-export([
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

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    gen_server:cast(?SERVER, {check_init}),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% @doc Init file checking
handle_cast({check_init}, State) ->
    {ok, CheckPeriod} = eppi_utl:get_env(new_packages_check_period),
    Files = eppi_pkg_stat:get_files(true),
    lager:info("* Monitoring: init with files: ~p, period: ~p secs",
                                            [Files, CheckPeriod/1000]),
    erlang:send_after(CheckPeriod, self(), {check_new_packages}),
    {noreply, State#state{files = Files, check_period = CheckPeriod}};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Check new files (periodic task)
handle_info({check_new_packages}, State) ->
    lager:info("* Monitoring: checking new files ..."),
    CurrFiles = eppi_pkg_stat:get_files(true),
    PrevFiles = State#state.files,
    % Try to get difference between files
    case CurrFiles -- PrevFiles of
        % No new files
        [] ->
            lager:info("* Monitoring: nothing new this time"),
            ok;
        % Got new files
        NewFiles ->
            lager:info("* Monitoring: found files: ~p", [NewFiles]),
            eppi_pkg_stat:notify_i_have(NewFiles)
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
