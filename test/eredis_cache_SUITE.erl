-module(eredis_cache_SUITE).
-author('Riccardo Massari <maxdrift85@gmail.com>').

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include("eredis_cache.hrl").
-include_lib("decorator_pt/include/decorator_pt.hrl").

-define(CACHE_VALIDITY, 1).

init_per_suite(Config) ->
    {ok, Apps} = application:ensure_all_started(eredis_cache),
    [{started_apps, Apps}|Config].

end_per_suite(Config) ->
    lists:foreach(fun(App) ->
                          application:stop(App)
                  end, ?config(started_apps, Config)).

all() ->
    [
     t_get_cached
    , t_get_expired
    ].

%% Tests

t_get_cached(_Config) ->
    Value = erlang:now(),
    {Value, Timestamp} = echo(Value),
    ok = timer:sleep(500 * ?CACHE_VALIDITY),
    {Value, Timestamp} = echo(Value),
    ok.

t_get_expired(_Config) ->
    Value = erlang:now(),
    {Value, Timestamp} = echo(Value),
    ok = timer:sleep(2000 * ?CACHE_VALIDITY),
    {Value, Timestamp2} = echo(Value),
    true = Timestamp < Timestamp2,
    ok.

%% Internal functions

?EREDIS_CACHE(eredis_cache_pool, [{validity, ?CACHE_VALIDITY}]).
echo(Value) ->
    Timestamp = erlang:now(),
    {Value, Timestamp}.
