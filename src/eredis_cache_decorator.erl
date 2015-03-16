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

eredis_cache_pt(Fun, Args, {Module, FunctionAtom, PoolName, Opts}) ->
    Key = get_key(Module, FunctionAtom, Args, PoolName, Opts),
    FromCache = eredis_cache:get(PoolName, Key),
    case FromCache of
        {ok, undefined} ->
            fun () ->
                    Res = Fun(Args),
                    ok = eredis_cache:set(PoolName, Key, Res, Opts),
                    Res
            end;
        {ok, Result} ->
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
