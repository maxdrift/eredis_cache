%%%-------------------------------------------------------------------
%%% @author Riccardo Massari <maxdrift85@gmail.com>
%%% @copyright (C) 2015 Riccardo Massari.
%%% @doc
%%% Erlang caching library based on Redis
%%% @end
%%%-------------------------------------------------------------------
-module(eredis_cache_decorator).
-author('Riccardo Massari <maxdrift85@gmail.com>').

-export([eredis_cache_pt/3, eredis_cache_inv_pt/3]).

-define(EREDIS_CACHE_FOLSOM_NAME(CacheName, Stat),
        <<"eredis_cache.stats.", CacheName/binary, ".", Stat/binary>>).

-spec eredis_cache_pt(function(), [term()], {atom(), atom(), atom(), [term()]}) ->
                             fun(() -> term()).
eredis_cache_pt(Fun, Args, {Module, FunctionAtom, PoolName, Opts}) ->
    {Prefix, Key} = get_key(Module, FunctionAtom, Args, PoolName, Opts),
    FullKey = <<Prefix/binary, Key/binary>>,
    FromCache = eredis_cache:get(PoolName, FullKey),
    case FromCache of
        {ok, undefined} ->
            quintana:notify_spiral(
              {?EREDIS_CACHE_FOLSOM_NAME(Prefix, <<"miss">>), 1}),
            lager:debug("Eredis cache miss.."),
            fun () ->
                    Res = Fun(Args),
                    CacheErrors = proplists:get_value(cache_errors, Opts, false),
                    case {Res, CacheErrors} of
                        {{error, _}, false} -> ok;
                        {R, _} -> ok = eredis_cache:set(PoolName, FullKey, R, Opts)
                    end,
                    Res
            end;
        {ok, Result} ->
            quintana:notify_spiral(
              {?EREDIS_CACHE_FOLSOM_NAME(Prefix, <<"hit">>), 1}),
            lager:debug("Eredis cache hit.."),
            fun() -> Result end;
        {error, no_connection} ->
            lager:warning("Eredis cache has no connection to Redis"),
            fun () -> Fun(Args) end;
        {error, Err} ->
            throw({error, {eredis_cache_pt, Err}})
    end.

-spec eredis_cache_inv_pt(function(), [term()], {atom(), atom(), atom(), [term()]}) ->
                             fun(() -> term()).
eredis_cache_inv_pt(Fun, Args, {Module, _FunctionAtom, PoolName, Opts}) ->
    Pattern = proplists:get_value(pattern, Opts),
    fun () ->
            Res = Fun(Args),
            ok = exec_invalidation(Module, PoolName, Res, Pattern),
            Res
    end.

exec_invalidation(Module, PoolName, Res, Pattern) ->
    case Pattern of
        undefined ->
            ok;
        {F, 1} when is_atom(F) ->
            Value = apply(Module, F, [Res]),
            InvRes = eredis_cache:invalidate_pattern(PoolName, Value),
            ok = check_invalidation_result(InvRes);
        Pattern when is_binary(Pattern) ->
            InvRes = eredis_cache:invalidate_pattern(PoolName, Pattern),
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
    Prefix = proplists:get_value(key_prefix, Opts, <<>>),
    K = case proplists:get_value(custom_key, Opts) of
            Module when is_atom(Module) ->
                apply(Module, generate_key, [PoolName, Module, FunctionAtom, Args]);
            {arg, N} when is_integer(N) ->
                lists:nth(N, Args);
            Custom when is_binary(Custom) ->
                Custom;
            undefined ->
                term_to_binary({decorated, Module, FunctionAtom, erlang:phash2(Args)})
        end,
    {Prefix, K}.
