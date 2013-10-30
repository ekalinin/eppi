-module(eppi_http_simple).

-behaviour(cowboy_http_handler).

-export([
    init/3,
    handle/2,
    terminate/3
]).

%%=============================================================================
%% Cowboy handler callback
%%=============================================================================

init({tcp, http}, Req, _Opts) ->
    {ok, Req, undefined_state}.

handle(Req, State) ->
    {Package, Req2} = cowboy_req:binding(package, Req),
    {ok, Response}  = handle_request(Req2, Package),
    {ok, Response, State}.

terminate(_Reason, _Req, _State) ->
    ok.

%%=============================================================================
%% Internal functions
%%=============================================================================

handle_request(Req, undefined) ->
    lager:debug("> handle_request: show packages ..."),
    {ok, Body} = simple_dtl:render([{packages, eppi_pkg_mon:get_packages()}]),
    cowboy_req:reply(200, [], Body, Req);

handle_request(Req, Package) ->
    lager:debug("> handle_request: show files for ~p ...", [Package]),
    {ok, Body} = simple_dtl:render([
        {files, eppi_pkg_mon:get_files(binary_to_list(Package))},
        {package_name, Package}
    ]),
    cowboy_req:reply(200, [], Body, Req).
