-module(eredis_cache_SUITE).
-author('Riccardo Massari <maxdrift85@gmail.com>').

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include("eredis_cache.hrl").
-include_lib("decorator_pt/include/decorator_pt.hrl").

-define(DEFAULT_POOL, eredis_cache_pool).
-define(DEFAULT_VALIDITY, 1).
-define(DEFAULT_PREFIX, <<"test_prefix.">>).
-define(DEFAULT_DEC_KEY, <<"custom_key">>).
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
    {ok, <<"OK">>} = eredis_pool:q(?DEFAULT_POOL, ["FLUSHDB"]),
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
    , t_decorator_prefix
    , t_decorator_custom_key
    , t_decorator_prefix_and_custom_key
    , t_decorator_invalidate
    ].

%% Tests

t_cached(Config) ->
    Key = ?config(key1, Config),
    Value = ?config(val1, Config),
    Opts = [{validity, ?DEFAULT_VALIDITY}],
    ok = eredis_cache:set(?DEFAULT_POOL, Key, Value, Opts),
    {ok, Value} = eredis_cache:get(?DEFAULT_POOL, Key).

t_expired(Config) ->
    Key = ?config(key1, Config),
    Value = ?config(val1, Config),
    Opts = [{validity, ?DEFAULT_VALIDITY}],
    ok = eredis_cache:set(?DEFAULT_POOL, Key, Value, Opts),
    ok = timer:sleep(2000 * ?DEFAULT_VALIDITY),
    {ok, undefined} = eredis_cache:get(?DEFAULT_POOL, Key),
    ok.

t_get_keys(Config) ->
    Key1 = ?config(key1, Config),
    Key2 = ?config(key2, Config),
    Key3 = ?config(key3, Config),
    Value = ?config(val1, Config),
    Opts = [{validity, 10 * ?DEFAULT_VALIDITY}],
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
    Opts = [{validity, 10 * ?DEFAULT_VALIDITY}],
    ok = eredis_cache:set(?DEFAULT_POOL, Key, Value, Opts),
    ok = eredis_cache:invalidate(?DEFAULT_POOL, Key),
    {ok, undefined} = eredis_cache:get(?DEFAULT_POOL, Key),
    ok.

t_invalidate_multiple(Config) ->
    Key1 = ?config(key1, Config),
    Key2 = ?config(key2, Config),
    Value = ?config(val1, Config),
    Opts = [{validity, 10 * ?DEFAULT_VALIDITY}],
    ok = eredis_cache:set(?DEFAULT_POOL, Key1, Value, Opts),
    ok = eredis_cache:set(?DEFAULT_POOL, Key2, Value, Opts),
    ok = eredis_cache:invalidate(?DEFAULT_POOL, [Key1, Key2]),
    {ok, undefined} = eredis_cache:get(?DEFAULT_POOL, Key1),
    {ok, undefined} = eredis_cache:get(?DEFAULT_POOL, Key2),
    ok.

t_invalidate_pattern(Config) ->
    Key1 = ?config(key1, Config),
    Key2 = ?config(key2, Config),
    Key3 = ?config(key3, Config),
    Value = ?config(val1, Config),
    Opts = [{validity, 10 * ?DEFAULT_VALIDITY}],
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
    ok = timer:sleep(500 * ?DEFAULT_VALIDITY),
    {Value, Timestamp} = echo(Value),
    ok.

t_decorator_expired(_Config) ->
    Value = erlang:now(),
    {Value, Timestamp} = echo(Value),
    ok = timer:sleep(2000 * ?DEFAULT_VALIDITY),
    {Value, Timestamp2} = echo(Value),
    true = Timestamp < Timestamp2,
    ok.

t_decorator_prefix(_Config) ->
    Value = erlang:now(),
    {Value, Timestamp} = echo2(Value),
    ok = timer:sleep(500 * ?DEFAULT_VALIDITY),
    Prefix = ?DEFAULT_PREFIX,
    PrefixSize = byte_size(Prefix),
    Pattern = << Prefix/binary, <<"*">>/binary >>,
    [Expected] = eredis_cache:get_keys(?DEFAULT_POOL, Pattern),
    << Prefix:PrefixSize/binary, _/binary >> = Expected,
    {Value, Timestamp} = echo2(Value),
    ok.

t_decorator_custom_key(_Config) ->
    Value = erlang:now(),
    {Value, Timestamp} = echo3(Value),
    ok = timer:sleep(500 * ?DEFAULT_VALIDITY),
    [?DEFAULT_DEC_KEY] = eredis_cache:get_keys(?DEFAULT_POOL, ?DEFAULT_DEC_KEY),
    {Value, Timestamp} = echo3(Value),
    ok.

t_decorator_prefix_and_custom_key(_Config) ->
    Value = erlang:now(),
    {Value, Timestamp} = echo4(Value),
    ok = timer:sleep(500 * ?DEFAULT_VALIDITY),
    FinalKey = << ?DEFAULT_PREFIX/binary, ?DEFAULT_DEC_KEY/binary >>,
    [FinalKey] = eredis_cache:get_keys(?DEFAULT_POOL, FinalKey),
    {Value, Timestamp} = echo4(Value),
    ok.

t_decorator_invalidate(_Config) ->
    Value = erlang:now(),
    {Value, Timestamp} = echo2(Value),
    ok = timer:sleep(100 * ?DEFAULT_VALIDITY),
    Prefix = ?DEFAULT_PREFIX,
    PrefixSize = byte_size(Prefix),
    Pattern = << Prefix/binary, <<"*">>/binary >>,
    [Expected] = eredis_cache:get_keys(?DEFAULT_POOL, Pattern),
    << Prefix:PrefixSize/binary, _/binary >> = Expected,
    {ok, <<"something">>} = set_something(<<"something">>),
    {Value, Timestamp2} = echo2(Value),
    true = Timestamp < Timestamp2,
    ok.

%% Internal functions

?EREDIS_CACHE(?DEFAULT_POOL, [{validity, ?DEFAULT_VALIDITY}]).
echo(Value) ->
    Timestamp = erlang:now(),
    {Value, Timestamp}.

?EREDIS_CACHE(?DEFAULT_POOL, [{validity, ?DEFAULT_VALIDITY},
                              {key_prefix, ?DEFAULT_PREFIX}]).
echo2(Value) ->
    Timestamp = erlang:now(),
    {Value, Timestamp}.

?EREDIS_CACHE(?DEFAULT_POOL, [{validity, ?DEFAULT_VALIDITY},
                              {custom_key, ?DEFAULT_DEC_KEY}]).
echo3(Value) ->
    Timestamp = erlang:now(),
    {Value, Timestamp}.

?EREDIS_CACHE(?DEFAULT_POOL, [{validity, ?DEFAULT_VALIDITY},
                              {custom_key, ?DEFAULT_DEC_KEY},
                              {key_prefix, ?DEFAULT_PREFIX}]).
echo4(Value) ->
    Timestamp = erlang:now(),
    {Value, Timestamp}.

?EREDIS_CACHE_INVALIDATE(?DEFAULT_POOL,
                         [{pattern, <<?DEFAULT_PREFIX/binary, <<"*">>/binary>>}]).
set_something(Value) ->
    {ok, Value}.

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
     {key3, wrap_value(<<"notquitefoo">>)},
     {val1, wrap_value(?DEFAULT_VAL)}
     | Config].
