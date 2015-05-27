%%%-------------------------------------------------------------------
%%% @author Riccardo Massari <maxdrift85@gmail.com>
%%% @copyright (C) 2015 Riccardo Massari.
%%% @doc
%%% Erlang caching library based on Redis
%%% @end
%%%-------------------------------------------------------------------
-module(eredis_cache_decorator).
-author('Riccardo Massari <maxdrift85@gmail.com>').

-include_lib("eredis_cache/include/eredis_cache.hrl").

-export([eredis_cache_pt/3, eredis_cache_inv_pt/3]).

-spec eredis_cache_pt(function(), [term()], {atom(), atom(), atom(), [term()]}) ->
                             fun(() -> term()).
eredis_cache_pt(Fun, Args, {Module, FunctionAtom, PoolName, Opts}) ->
    Prefix = proplists:get_value(key_prefix, Opts, <<>>),
    Timer = quintana:begin_timed(?EREDIS_CACHE_FOLSOM_NAME(Prefix, <<"get_latency">>)),
    Key = get_key(Module, FunctionAtom, Args, PoolName, Opts),
    FullKey = <<Prefix/binary, Key/binary>>,
    Timeout = proplists:get_value(timeout, Opts, ?TIMEOUT),
    FromCache = eredis_cache:get(PoolName, FullKey, Timeout),
    case FromCache of
        {ok, undefined} ->
            quintana:notify_spiral(
              {?EREDIS_CACHE_FOLSOM_NAME(Prefix, <<"miss">>), 1}),
            fun () ->
                    Res = Fun(Args),
                    CacheErrors = proplists:get_value(cache_errors, Opts, false),
                    case {Res, CacheErrors} of
                        {{error, _}, false} -> ok;
                        {error, false} -> ok;
                        {R, _} ->
                            Val = proplists:get_value(validity, Opts, ?DEF_VALIDITY),
                            ok = eredis_cache:set(PoolName, FullKey, R, Val, Timeout)
                    end,
                    ok = quintana:notify_timed(Timer),
                    Res
            end;
        {ok, Result} ->
            quintana:notify_spiral(
              {?EREDIS_CACHE_FOLSOM_NAME(Prefix, <<"hit">>), 1}),
            fun() ->
                    ok = quintana:notify_timed(Timer),
                    Result
            end;
        {error, no_connection} ->
            lager:warning("Eredis cache has no connection to Redis"),
            fun () ->
                    Result = Fun(Args),
                    ok = quintana:notify_timed(Timer),
                    Result
            end;
        {error, Err} ->
            quintana:notify_spiral(
              {?EREDIS_CACHE_FOLSOM_NAME(Prefix, <<"get_error">>), 1}),
            ok = quintana:notify_timed(Timer),
            throw({error, {eredis_cache_pt, Err}})
    end.

-spec eredis_cache_inv_pt(function(), [term()], {atom(), atom(), atom(), [term()]}) ->
                             fun(() -> term()).
eredis_cache_inv_pt(Fun, Args, {Module, _FunctionAtom, PoolName, Opts}) ->
    Prefix = proplists:get_value(key_prefix, Opts, <<>>),
    Pattern = proplists:get_value(pattern, Opts),
    Timeout = proplists:get_value(timeout, Opts, ?TIMEOUT),
    fun () ->
            Res = Fun(Args),
            ok = exec_invalidation(Module, PoolName, Res, Prefix, Pattern, Timeout),
            Res
    end.

exec_invalidation(Module, PoolName, Res, Prefix, Pattern, Timeout) ->
    case Pattern of
        undefined ->
            ok;
        {F, 1} when is_atom(F) ->
            Value = apply(Module, F, [Res]),
            InvRes = eredis_cache:invalidate_pattern(PoolName, Prefix, Value, Timeout),
            ok = check_invalidation_result(InvRes);
        Pattern when is_binary(Pattern) ->
            InvRes = eredis_cache:invalidate_pattern(PoolName, Prefix, Pattern, Timeout),
            ok = check_invalidation_result(InvRes)
    end.

check_invalidation_result(Result) ->
    case Result of
        ok ->
            ok;
        {error, no_connection} ->
            lager:warning("Eredis cache has no connection to Redis"),
            ok;
        {error, Err} ->
            throw({error, {eredis_cache_inv_pt, Err}})
    end.

get_key(Module, FunctionAtom, Args, PoolName, Opts) ->
    case proplists:get_value(custom_key, Opts) of
        Module when is_atom(Module) ->
            apply(Module, generate_key, [PoolName, Module, FunctionAtom, Args]);
        {arg, N} when is_integer(N) ->
            Arg = lists:nth(N, Args),
            case is_binary(Arg) of
                true -> Arg;
                false -> integer_to_binary(erlang:phash2(Arg))
            end;
        Custom when is_binary(Custom) ->
            Custom;
        undefined ->
            term_to_binary({decorated, Module, FunctionAtom, erlang:phash2(Args)})
    end.
