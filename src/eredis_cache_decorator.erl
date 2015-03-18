%%%-------------------------------------------------------------------
%%% @author Riccardo Massari <maxdrift85@gmail.com>
%%% @copyright (C) 2015 Riccardo Massari.
%%% @doc
%%% Erlang caching library based on Redis
%%% @end
%%%-------------------------------------------------------------------
-module(eredis_cache_decorator).
-author('Riccardo Massari <maxdrift85@gmail.com>').

-export([eredis_cache_pt/3]).

-spec eredis_cache_pt(function(), [term()], {atom(), atom(), atom(), [term()]}) ->
                             fun(() -> term()).
eredis_cache_pt(Fun, Args, {Module, FunctionAtom, PoolName, Opts}) ->
    Key = get_key(Module, FunctionAtom, Args, PoolName, Opts),
    FromCache = eredis_cache:get(PoolName, Key),
    case FromCache of
        {ok, undefined} ->
            lager:info("Miss.."),
            fun () ->
                    Res = Fun(Args),
                    CacheErrors = proplists:get_value(cache_errors, Opts, false),
                    case {Res, CacheErrors} of
                        {{error, _}, false} -> ok;
                        {R, _} -> ok = eredis_cache:set(PoolName, Key, R, Opts)
                    end,
                    Res
            end;
        {ok, Result} ->
            lager:info("Hit.."),
            fun() -> Result end;
        {error, Err} ->
            throw({error, {eredis_cache_pt, Err}})
    end.

get_key(Module, FunctionAtom, Args, PoolName, Opts) ->
    case proplists:get_value(custom_key, Opts) of
        Module when is_atom(Module) ->
            apply(Module, generate_key, [PoolName, Module, FunctionAtom, Args]);
        undefined ->
            {decorated, Module, FunctionAtom, erlang:phash2(Args)};
        {arg, N} when is_integer(N) ->
            lists:nth(N, Args)
    end.
