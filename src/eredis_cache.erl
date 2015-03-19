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

-export([start/0, stop/0]).

-export([
         get/2,
         set/4,
         get_keys/2,
         invalidate/2,
         invalidate_pattern/2
        ]).

start() ->
    application:start(?MODULE).

stop() ->
    application:stop(?MODULE).

get(PoolName, Key) ->
    case eredis_pool:q(PoolName, ["GET", Key]) of
        {ok, undefined}=R -> R;
        {ok, Value} -> {ok, binary_to_term(Value)};
        {error, _}=E -> E
    end.

set(PoolName, Key, Value, Opts) ->
    BinValue = term_to_binary(Value),
    Validity = proplists:get_value(validity, Opts, ?DEF_VALIDITY),
    Fun = fun(Worker) ->
                  eredis:q(Worker, ["SET", Key, BinValue]),
                  eredis:q(Worker, ["EXPIRE", Key, Validity])
          end,
    {ok, [<<"OK">>, <<"1">>]} = eredis_pool:transaction(PoolName, Fun),
    ok.

get_keys(PoolName, Pattern) ->
    {ok, Keys} = eredis_pool:q(PoolName, ["KEYS", Pattern]),
    Keys.

invalidate(PoolName, Keys) when is_list(Keys) ->
    lists:foreach(fun(Key) ->
                          ok = invalidate(PoolName, Key)
                  end, Keys);
invalidate(PoolName, Key) when is_binary(Key) ->
    {ok, _} = eredis_pool:q(PoolName, ["DEL", Key]),
    ok.

invalidate_pattern(PoolName, Pattern) ->
    {ok, [NextCursor, NextKeys]} = eredis_pool:q(PoolName,
                                                 ["SCAN", 0, "MATCH", Pattern]),
    invalidate_pattern(PoolName, Pattern, NextCursor, NextKeys).

invalidate_pattern(PoolName, _Pattern, <<"0">>, Keys) ->
    invalidate(PoolName, Keys);
invalidate_pattern(PoolName, Pattern, Cursor, Keys) ->
    invalidate(PoolName, Keys),
    {ok, [NextCursor, NextKeys]} = eredis_pool:q(PoolName,
                                                 ["SCAN", Cursor, "MATCH", Pattern]),
    invalidate_pattern(PoolName, Pattern, NextCursor, NextKeys).
