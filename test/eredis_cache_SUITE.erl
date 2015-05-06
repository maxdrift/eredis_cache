-module(eredis_cache_SUITE).
-author('Riccardo Massari <maxdrift85@gmail.com>').

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eredis_cache/include/eredis_cache.hrl").
-include_lib("decorator_pt/include/decorator_pt.hrl").

-define(DEFAULT_POOL, eredis_cache_pool).
-define(NO_REDIS_POOL, eredis_cache_pool_no_redis).
-define(RUNTIME_POOL, eredis_cache_runtime_pool).
-define(DEFAULT_VALIDITY, 1).
-define(DEFAULT_PREFIX, <<"test_prefix.">>).
-define(DEFAULT_DEC_KEY, <<"custom_key">>).
-define(DEFAULT_KEY, <<"foo">>).
-define(DEFAULT_VAL, <<"bar">>).

init_per_suite(Config) ->
    {ok, Apps} = application:ensure_all_started(eredis_cache),
    [{started_apps, Apps}|Config].

end_per_suite(_Config) ->
    ok.

init_per_testcase(TestCase, Config) ->
    create_test_vars(TestCase, Config).

end_per_testcase(_TestCase, _Config) ->
    {ok, <<"OK">>} = eredis_pool:q(?DEFAULT_POOL, ["FLUSHDB"]),
    ok.

groups() ->
    [
     {cache_apis, [],
      [
       t_cached
      , t_expired
      , t_get_keys
      , t_invalidate_single
      , t_invalidate_multiple
      , t_invalidate_pattern
      ]},
     {decorators, [],
      [
       t_decorator_cached
      , t_decorator_expired
      , t_decorator_prefix
      , t_decorator_custom_key
      , t_decorator_prefix_and_custom_key
      , t_decorator_invalidate
      , t_decorator_invalidate_custom_fun
      ]},
     {start_stop_cache, [],
      [
       t_start_stop_cache_custom_params
      , t_start_stop_cache_default_params
      ]},
     {redis_offline_on_startup, [],
      [
       t_apis_not_crashing
      , t_cache_decorator_not_crashing
      , t_invalidate_decorator_not_crashing
      , t_create_fake_cache
      ]}
    ].

all() ->
    [
     {group, cache_apis},
     {group, decorators},
     {group, start_stop_cache},
     {group, redis_offline_on_startup}
    ].

%% APIs

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
    ok = timer:sleep(timer:seconds(?DEFAULT_VALIDITY * 2)),
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
    {ok, Keys} = eredis_cache:get_keys(?DEFAULT_POOL, <<"foo*">>),
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

%% Decorators

t_decorator_cached(_Config) ->
    Value = erlang:now(),
    {Value, Timestamp} = echo(Value),
    ok = timer:sleep(timer:seconds(?DEFAULT_VALIDITY div 2)),
    {Value, Timestamp} = echo(Value),
    ok.

t_decorator_expired(_Config) ->
    Value = erlang:now(),
    {Value, Timestamp} = echo(Value),
    ok = timer:sleep(timer:seconds(?DEFAULT_VALIDITY * 2)),
    {Value, Timestamp2} = echo(Value),
    true = Timestamp < Timestamp2,
    ok.

t_decorator_prefix(_Config) ->
    Value = erlang:now(),
    {Value, Timestamp} = echo2(Value),
    ok = timer:sleep(timer:seconds(?DEFAULT_VALIDITY div 2)),
    Prefix = ?DEFAULT_PREFIX,
    PrefixSize = byte_size(Prefix),
    Pattern = <<Prefix/binary, "*">>,
    {ok, [Expected]} = eredis_cache:get_keys(?DEFAULT_POOL, Pattern),
    << Prefix:PrefixSize/binary, _/binary >> = Expected,
    {Value, Timestamp} = echo2(Value),
    ok.

t_decorator_custom_key(_Config) ->
    Value = erlang:now(),
    {Value, Timestamp} = echo3(Value),
    ok = timer:sleep(timer:seconds(?DEFAULT_VALIDITY div 2)),
    {ok, [?DEFAULT_DEC_KEY]} = eredis_cache:get_keys(?DEFAULT_POOL, ?DEFAULT_DEC_KEY),
    {Value, Timestamp} = echo3(Value),
    ok.

t_decorator_prefix_and_custom_key(_Config) ->
    Value = erlang:now(),
    {Value, Timestamp} = echo4(Value),
    ok = timer:sleep(timer:seconds(?DEFAULT_VALIDITY div 2)),
    FinalKey = << ?DEFAULT_PREFIX/binary, ?DEFAULT_DEC_KEY/binary >>,
    {ok, [FinalKey]} = eredis_cache:get_keys(?DEFAULT_POOL, FinalKey),
    {Value, Timestamp} = echo4(Value),
    ok.

t_decorator_invalidate(_Config) ->
    Value = erlang:now(),
    {Value, Timestamp} = echo2(Value),
    Prefix = ?DEFAULT_PREFIX,
    PrefixSize = byte_size(Prefix),
    Pattern = <<Prefix/binary, "*">>,
    {ok, [Expected]} = eredis_cache:get_keys(?DEFAULT_POOL, Pattern),
    << Prefix:PrefixSize/binary, _/binary >> = Expected,
    {ok, <<"something">>} = set_something(<<"something">>),
    {Value, Timestamp2} = echo2(Value),
    true = Timestamp < Timestamp2,
    ok.

t_decorator_invalidate_custom_fun(_Config) ->
    Value = erlang:now(),
    {Value, Timestamp} = echo2(Value),
    Prefix = ?DEFAULT_PREFIX,
    PrefixSize = byte_size(Prefix),
    Pattern = <<Prefix/binary, "*">>,
    {ok, [Expected]} = eredis_cache:get_keys(?DEFAULT_POOL, Pattern),
    << Prefix:PrefixSize/binary, _/binary >> = Expected,
    {ok, ?DEFAULT_PREFIX} = set_something2(?DEFAULT_PREFIX),
    {Value, Timestamp2} = echo2(Value),
    true = Timestamp < Timestamp2,
    ok.

%% Start/Stop cache

t_start_stop_cache_custom_params(Config) ->
    Host = "localhost",
    Port = 6379,
    Password = "",
    Size = 1,
    MaxOverflow = 0,
    RequireRedis = true,
    SizeArgs = {Size, MaxOverflow},
    WorkerArgs = [
                  {host, Host},
                  {port, Port},
                  {password, Password},
                  {require_redis_on_start, RequireRedis}
                 ],
    {ok, _} = eredis_cache:start_cache(?RUNTIME_POOL, SizeArgs, WorkerArgs),
    Key = ?config(key1, Config),
    Value = ?config(val1, Config),
    Opts = [{validity, ?DEFAULT_VALIDITY}],
    ok = eredis_cache:set(?RUNTIME_POOL, Key, Value, Opts),
    {ok, Value} = eredis_cache:get(?RUNTIME_POOL, Key),
    ok = eredis_cache:stop_cache(?RUNTIME_POOL).

t_start_stop_cache_default_params(Config) ->
    {ok, _} = eredis_cache:start_cache(?RUNTIME_POOL, {1, 0}, []),
    Key = ?config(key1, Config),
    Value = ?config(val1, Config),
    Opts = [{validity, ?DEFAULT_VALIDITY}],
    ok = eredis_cache:set(?RUNTIME_POOL, Key, Value, Opts),
    {ok, Value} = eredis_cache:get(?RUNTIME_POOL, Key),
    ok = eredis_cache:stop_cache(?RUNTIME_POOL).

%% Redis offline on startup

t_apis_not_crashing(Config) ->
    Key = ?config(key1, Config),
    Value = ?config(val1, Config),
    Opts = [{validity, ?DEFAULT_VALIDITY}],
    {error, no_connection} = eredis_cache:set(?NO_REDIS_POOL, Key, Value, Opts),
    {error, no_connection} = eredis_cache:get(?NO_REDIS_POOL, Key).

t_cache_decorator_not_crashing(_Config) ->
    Value = erlang:now(),
    {Value, Timestamp} = echo5(Value),
    ok = timer:sleep(timer:seconds(?DEFAULT_VALIDITY div 2)),
    {Value, Timestamp2} = echo5(Value),
    true = Timestamp < Timestamp2.

t_invalidate_decorator_not_crashing(_Config) ->
    Value = erlang:now(),
    {Value, Timestamp} = echo5(Value),
    {ok, <<"something">>} = set_something3(<<"something">>),
    {Value, Timestamp2} = echo5(Value),
    true = Timestamp < Timestamp2.

t_create_fake_cache(Config) ->
    Host = "cache.intentionally.disabled",
    Size = 1,
    MaxOverflow = 0,
    RequireRedis = false,
    SizeArgs = {Size, MaxOverflow},
    WorkerArgs = [
                  {host, Host},
                  {require_redis_on_start, RequireRedis}
                 ],
    {ok, _} = eredis_cache:start_cache(?RUNTIME_POOL, SizeArgs, WorkerArgs),
    Key = ?config(key1, Config),
    Value = ?config(val1, Config),
    Opts = [{validity, ?DEFAULT_VALIDITY}],
    {error, no_connection} = eredis_cache:set(?RUNTIME_POOL, Key, Value, Opts),
    {error, no_connection} = eredis_cache:get(?RUNTIME_POOL, Key).

%% Decorated setters

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

?EREDIS_CACHE(?NO_REDIS_POOL, [{validity, ?DEFAULT_VALIDITY}]).
echo5(Value) ->
    Timestamp = erlang:now(),
    {Value, Timestamp}.

%% Decorated setters

?EREDIS_CACHE_INVALIDATE(?DEFAULT_POOL, [{key_prefix, ?DEFAULT_PREFIX},
                                         {pattern, <<"*">>}]).
set_something(Value) ->
    {ok, Value}.

?EREDIS_CACHE_INVALIDATE(?DEFAULT_POOL,
                         [{pattern, get_pattern/1}]).
set_something2(Value) ->
    {ok, Value}.

get_pattern({ok, Result}) ->
    <<Result/binary, "*">>.

?EREDIS_CACHE_INVALIDATE(?NO_REDIS_POOL,
                         [{pattern, <<?DEFAULT_PREFIX/binary, "*">>}]).
set_something3(Value) ->
    {ok, Value}.

%% Internal functions

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
