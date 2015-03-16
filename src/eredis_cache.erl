%%%-------------------------------------------------------------------
%%% @author Riccardo Massari <maxdrift85@gmail.com>
%%% @copyright (C) 2015 Riccardo Massari.
%%% @doc
%%% Erlang caching library based on Redis
%%% @end
%%%-------------------------------------------------------------------
-module(eredis_cache).
-author('Riccardo Massari <maxdrift85@gmail.com>').

-include("eredis_cache.hrl").

-export([
         get/2,
         set/4,
         invalidate/2
        ]).

get(PoolName, Key) ->
    case eredis_pool:q(PoolName, ["GET", Key]) of
        {ok, undefined}=R -> R;
        {ok, Value} -> {ok, binary_to_term(Value)};
        {error, _}=E -> E
    end.

set(PoolName, Key, Value, Opts) ->
    BinValue = term_to_binary(Value),
    Validity = case proplists:get_value(validity, Opts) of
                   undefined -> ?DEF_VALIDITY;
                   Val -> Val
               end,
    [{ok, <<"OK">>},
     {ok, <<"1">>}] = eredis_pool:qp(PoolName,
                                     [["SET", Key, BinValue],
                                      ["EXPIRE", Key, Validity]]),
    ok.

invalidate(PoolName, Key) ->
    {ok, <<"1">>} = eredis_pool:q(PoolName, ["EXPIRE", Key, 0]),
    ok.
