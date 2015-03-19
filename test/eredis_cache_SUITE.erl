-module(eredis_cache_SUITE).
-author('Riccardo Massari <maxdrift85@gmail.com>').

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include("eredis_cache.hrl").
-include_lib("decorator_pt/include/decorator_pt.hrl").

-define(DEFAULT_POOL, eredis_cache_pool).
-define(CACHE_VALIDITY, 1).
-define(DEFAULT_KEY, <<"foo">>).
-define(DEFAULT_VAL, <<"bar">>).

init_per_suite(Config) ->
    {ok, Apps} = application:ensure_all_started(eredis_cache),
    [{started_apps, Apps}|Config].

end_per_suite(Config) ->
    lists:foreach(fun(App) ->
                          application:stop(App)
                  end, ?config(started_apps, Config)).

init_per_testcase(TestCase, Config) ->
    create_test_vars(TestCase, Config).

end_per_testcase(_TestCase, _Config) ->
    ok.

all() ->
    [
     t_cached
    , t_expired
    , t_get_keys
    , t_invalidate_single
    , t_invalidate_multiple
    , t_invalidate_pattern
    , t_decorator_cached
    , t_decorator_expired
    ].

%% Tests

t_cached(Config) ->
    Key = ?config(key1, Config),
    Value = ?config(val1, Config),
    Opts = [{validity, ?CACHE_VALIDITY}],
    ok = eredis_cache:set(?DEFAULT_POOL, Key, Value, Opts),
    {ok, Value} = eredis_cache:get(?DEFAULT_POOL, Key).

t_expired(Config) ->
    Key = ?config(key1, Config),
    Value = ?config(val1, Config),
    Opts = [{validity, ?CACHE_VALIDITY}],
    ok = eredis_cache:set(?DEFAULT_POOL, Key, Value, Opts),
    ok = timer:sleep(2000 * ?CACHE_VALIDITY),
    {ok, undefined} = eredis_cache:get(?DEFAULT_POOL, Key),
    ok.

t_get_keys(Config) ->
    Key1 = ?config(key1, Config),
    Key2 = ?config(key2, Config),
    Key3 = <<"notquitefoo">>,
    Value = ?config(val1, Config),
    Opts = [{validity, 10 * ?CACHE_VALIDITY}],
    ok = eredis_cache:set(?DEFAULT_POOL, Key1, Value, Opts),
    ok = eredis_cache:set(?DEFAULT_POOL, Key2, Value, Opts),
    ok = eredis_cache:set(?DEFAULT_POOL, Key3, Value, Opts),
    Keys = eredis_cache:get_keys(?DEFAULT_POOL, <<"foo*">>),
    true = lists:member(Key1, Keys),
    true = lists:member(Key2, Keys),
    false = lists:member(Key3, Keys),
    ok.

t_invalidate_single(Config) ->
    Key = ?config(key1, Config),
    Value = ?config(val1, Config),
    Opts = [{validity, 10 * ?CACHE_VALIDITY}],
    ok = eredis_cache:set(?DEFAULT_POOL, Key, Value, Opts),
    ok = eredis_cache:invalidate(?DEFAULT_POOL, Key),
    {ok, undefined} = eredis_cache:get(?DEFAULT_POOL, Key),
    ok.

t_invalidate_multiple(Config) ->
    Key1 = ?config(key1, Config),
    Key2 = ?config(key2, Config),
    Value = ?config(val1, Config),
    Opts = [{validity, 10 * ?CACHE_VALIDITY}],
    ok = eredis_cache:set(?DEFAULT_POOL, Key1, Value, Opts),
    ok = eredis_cache:set(?DEFAULT_POOL, Key2, Value, Opts),
    ok = eredis_cache:invalidate(?DEFAULT_POOL, [Key1, Key2]),
    {ok, undefined} = eredis_cache:get(?DEFAULT_POOL, Key1),
    {ok, undefined} = eredis_cache:get(?DEFAULT_POOL, Key2),
    ok.

t_invalidate_pattern(Config) ->
    Key1 = ?config(key1, Config),
    Key2 = ?config(key2, Config),
    Key3 = <<"notquitefoo">>,
    Value = ?config(val1, Config),
    Opts = [{validity, 10 * ?CACHE_VALIDITY}],
    ok = eredis_cache:set(?DEFAULT_POOL, Key1, Value, Opts),
    ok = eredis_cache:set(?DEFAULT_POOL, Key2, Value, Opts),
    ok = eredis_cache:set(?DEFAULT_POOL, Key3, Value, Opts),
    ok = eredis_cache:invalidate_pattern(?DEFAULT_POOL, <<"foo*">>),
    {ok, undefined} = eredis_cache:get(?DEFAULT_POOL, Key1),
    {ok, undefined} = eredis_cache:get(?DEFAULT_POOL, Key2),
    {ok, Value} = eredis_cache:get(?DEFAULT_POOL, Key3),
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

?EREDIS_CACHE(?DEFAULT_POOL, [{validity, ?CACHE_VALIDITY}]).
echo(Value) ->
    Timestamp = erlang:now(),
    {Value, Timestamp}.

timestamp_usecs() ->
    {Megasecs, Secs, Microsecs} = os:timestamp(),
    Megasecs * 1000000000000 + Secs * 1000000 + Microsecs.

wrap_value(Val) ->
    Ts = timestamp_usecs(),
    <<_:16, TsBin/binary>> = integer_to_binary(Ts),
    <<Val/binary, TsBin/binary>>.

%% Test setup/teardown

create_test_vars(_TestCase, Config) ->
    [{key1, wrap_value(?DEFAULT_KEY)},
     {key2, wrap_value(?DEFAULT_KEY)},
     {key3, wrap_value(?DEFAULT_KEY)},
     {key4, wrap_value(?DEFAULT_KEY)},
     {val1, wrap_value(?DEFAULT_VAL)}
     | Config].
