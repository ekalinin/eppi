-module(eppi_utl).

-compile([export_all]).

get_env(EnvName) ->
    application:get_env(eppi, EnvName).

get_env(EnvName, DefaultValue) ->
    case get_env(EnvName) of
        {_, EnvValue} ->
            EnvValue;
        _ ->
            DefaultValue
    end.

get_timestamp() ->
    {_, _Secs, MicroSecs} = now(),
    MicroSecs.

get_allowed_timestamp(LastTimeStamp) ->
    {ok, Interval} = eppi_utl:get_env(min_interval_between_checks),
    lager:debug("+ Last check: ~p, Interval: ~p", [LastTimeStamp, Interval]),
    (LastTimeStamp + Interval).
