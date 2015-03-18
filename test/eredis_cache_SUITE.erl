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
     t_cached
    , t_expired
    , t_invalidate_single
    , t_invalidate_multiple
    , t_decorator_cached
    , t_decorator_expired
    ].

%% Tests

t_cached(_Config) ->
    Key = <<"foo">>,
    Value = <<"bar">>,
    Opts = [{validity, ?CACHE_VALIDITY}],
    ok = eredis_cache:set(eredis_cache_pool, Key, Value, Opts),
    {ok, Value} = eredis_cache:get(eredis_cache_pool, Key).

t_expired(_Config) ->
    Key = <<"foo">>,
    Value = <<"bar">>,
    Opts = [{validity, ?CACHE_VALIDITY}],
    ok = eredis_cache:set(eredis_cache_pool, Key, Value, Opts),
    ok = timer:sleep(2000 * ?CACHE_VALIDITY),
    {ok, undefined} = eredis_cache:get(eredis_cache_pool, Key),
    ok.

t_invalidate_single(_Config) ->
    Key = <<"foo">>,
    Value = <<"bar">>,
    Opts = [{validity, 10 * ?CACHE_VALIDITY}],
    ok = eredis_cache:set(eredis_cache_pool, Key, Value, Opts),
    ok = eredis_cache:invalidate(eredis_cache_pool, Key),
    {ok, undefined} = eredis_cache:get(eredis_cache_pool, Key),
    ok.

t_invalidate_multiple(_Config) ->
    Key1 = <<"foo1">>,
    Key2 = <<"foo2">>,
    Value = <<"bar">>,
    Opts = [{validity, 10 * ?CACHE_VALIDITY}],
    ok = eredis_cache:set(eredis_cache_pool, Key1, Value, Opts),
    ok = eredis_cache:set(eredis_cache_pool, Key2, Value, Opts),
    ok = eredis_cache:invalidate(eredis_cache_pool, [Key1, Key2]),
    {ok, undefined} = eredis_cache:get(eredis_cache_pool, Key1),
    {ok, undefined} = eredis_cache:get(eredis_cache_pool, Key2),
    ok.

t_decorator_cached(_Config) ->
    Value = erlang:now(),
    {Value, Timestamp} = echo(Value),
    ok = timer:sleep(500 * ?CACHE_VALIDITY),
    {Value, Timestamp} = echo(Value),
    ok.

t_decorator_expired(_Config) ->
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
