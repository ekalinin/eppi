-module(eppi).

%% API
-export([
    start/0,
    stop/0
]).

-define(APPS, [lager, crypto, ranch, cowboy, eppi]).

%% ===================================================================
%% API functions
%% ===================================================================

start() ->
    [application:start(A) || A <- ?APPS],
    ok.

stop() ->
    [application:stop(A) || A <- lists:reverse(?APPS)],
    ok.
