# Eredis Cache [![Build Status](https://travis-ci.org/maxdrift/eredis_cache.svg?branch=master)](https://travis-ci.org/maxdrift/eredis_cache)

### Summary
This library aims to create an easy way to add Redis-based caching to erlang applications.
The caching mechanism is built to be transparent in case of failures in the caching backend (e.g. Redis server) not altering the cached function behaviour except from generating some warning logs.

### Basic usage
0) Add `eredis_cache` to the list of required applications in your `.app.src` file;

1) Set up a pool of workers to serve caching requests;
```erlang
SizeArgs = {10, %% size of the eredis clients pool
            0}, %% overflow of the pool
WorkerArgs = [
              {host, localhost}, %% Redis server host
              {port, 6379}, %% Redis server port
              {require_redis_on_start, false} %% Don't crash if Redis is not running
             ],
{ok, _} = eredis_cache:start_cache(your_eredis_pool_name, SizeArgs, WorkerArgs),
```

2) Include `eredis_cache` and `decorator_pt` header files in your module;
```erlang
-include_lib("eredis_cache/include/eredis_cache.hrl").
-include_lib("decorator_pt/include/decorator_pt.hrl").
```

3) Place the `?EREDIS_CACHE` decorator macro on top of the function you want to cache;
```erlang
?EREDIS_CACHE(your_eredis_pool_name, [{validity, 10}]).
sum(A, B) ->
    A + B.
```

Done! Now you've got caching for the `sum/2` function for **10 seconds**.

### Customization
There are several custom options you can pass to the `?EREDIS_CACHE` decorator macro such as:
- `{validity, 10} %% seconds` to set how many seconds the value should be stored in cache. [default: 60]
- `{custom_key, <<"some_key">>}` to customize how the caching key is generated; see [Custom caching keys](#custom-caching-keys). [default: `term_to_binary({decorated, Module, FunctionAtom, erlang:phash2(Args)})`]
- `{key_prefix, <<"some_prefix">>}`to easily keep track of your caching keys or group them. [default: `<<>>`]
- `{timeout, 5000} %% milliseconds` to set the redis client timeout. [default: 5000]
- `{pool_timeout, 5000} %% milliseconds` to set the redis client poolboy timeout. [default: 5000]
- `{cache_errors, true}` to force caching of error results; by default `error` and `{error, _}` results are not cached. [default: false]
- `{compression, 5} %% between 0 and 9` to set some level of compression for the cached values; internally it uses `term_to_binary/2` compression. [default: 0]

### Custom caching keys
The `custom_key` option enables some additional customizations on the way the caching key is generated:
- `{custom_key, module_name}` where `module_name` has to be the same module where the `?EREDIS_CACHE` decorator macro is used. Keys will be generated from the result of `module_name:generate_key/4` with arguments `[PoolName, Module, FunctionAtom, FunctionArgs]`.
- `{custom_key, {arg, 2}}` will generate caching keys from the *second* parameter passed to the cached function (hashing it if not binary).
- `{custom_key, {arg, {2, 3, 5}}}` will generate caching keys hashing the *second*, *third* and *fifth* parameter passed to the cached function.
- `{custom_key, <<"any_value">>}` will use the specified static key.

**key_prefix, if specified, is prepended to automatic keys as well as custom keys.**

### Contribution
Please feel free to fork this project, improve it and send pull requests.

### Credits
This library is inspired by **[erl-cache](https://github.com/spilgames/erl-cache)** and takes advantage of:
- [Redis](http://redis.io/)
- a [fork](https://github.com/maxdrift/eredis) of [eredis](https://github.com/wooga/eredis)
- a [fork](https://github.com/maxdrift/eredis_pool) of [eredis_pool](https://github.com/hiroeorz/eredis_pool/)
- [erl-decorator-pt](https://github.com/spilgames/erl-decorator-pt)
