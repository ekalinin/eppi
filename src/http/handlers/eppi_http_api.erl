-module(eppi_http_api).

-behaviour(cowboy_http_handler).

-export([
    init/3,
    handle/2,
    terminate/3
]).

init({tcp, http}, Req, _Opts) ->
    {ok, Req, undefined_state}.

% TODO: see realisation in Ybot/src/web/web_admin_req_handler.erl
%       http://erlang-russian.org/post/122
%       http://habrahabr.ru/post/173595/

handle(Req, State) ->
    Body = <<"<h1>It works!</h1>">>,
    {ok, Req2} = cowboy_req:reply(200, [], Body, Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.
