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
