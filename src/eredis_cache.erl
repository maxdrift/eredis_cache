%%%-------------------------------------------------------------------
%%% @author Riccardo Massari <maxdrift85@gmail.com>
%%% @copyright (C) 2015 Riccardo Massari.
%%% @doc
%%% Erlang caching library based on Redis
%%% @end
%%%-------------------------------------------------------------------
-module(eredis_cache).
-author('Riccardo Massari <maxdrift85@gmail.com>').

-include_lib("eredis_cache/include/eredis_cache.hrl").

-export([start_cache/3, stop_cache/1]).

-export([
         get/2,
         set/3,
         set/4,
         get_keys/2,
         invalidate/2,
         invalidate_pattern/2,
         invalidate_pattern/3
        ]).

-define(TIMEOUT, 15000).

start_cache(CacheName, PoolArgs, WorkerArgs) ->
    eredis_pool_sup:create_pool(CacheName, PoolArgs, WorkerArgs).

stop_cache(CacheName) ->
    eredis_pool_sup:delete_pool(CacheName).

get(PoolName, Key) when is_binary(Key) ->
    case eredis_pool:q(PoolName, ["GET", Key], ?TIMEOUT) of
        {ok, undefined}=R -> R;
        {ok, Value} -> {ok, binary_to_term(Value)};
        {error, _}=E -> E
    end.

set(PoolName, Key, Value) when is_binary(Key) ->
    set(PoolName, Key, Value, ?DEF_VALIDITY).

set(PoolName, Key, Value, Validity) when is_binary(Key),
                                         is_integer(Validity) ->
    BinValue = term_to_binary(Value),
    Fun = fun(Worker) ->
                  case eredis:q(Worker, ["SET", Key, BinValue]) of
                      {ok, <<"QUEUED">>} ->
                          case eredis:q(Worker, ["EXPIRE", Key, Validity]) of
                              {ok, <<"QUEUED">>} -> ok;
                              {error, _}=E -> E
                          end;
                      {error, _}=E -> E
                  end
          end,
    case eredis_pool:transaction(PoolName, Fun) of
        {ok, [<<"OK">>, <<"1">>]} -> ok;
        {error, _}=E -> E
    end.

get_keys(PoolName, Pattern) when is_binary(Pattern) ->
    eredis_pool:q(PoolName, ["KEYS", Pattern], ?TIMEOUT).

invalidate(_PoolName, []) ->
    ok;
invalidate(PoolName, [Key | Rest]) ->
    case invalidate(PoolName, Key) of
        ok ->
            invalidate(PoolName, Rest);
        {error, _}=E -> E
    end;
invalidate(PoolName, Key) when is_binary(Key) ->
    case eredis_pool:q(PoolName, ["DEL", Key], ?TIMEOUT) of
        {ok, _} -> ok;
        {error, _}=E -> E
    end.

invalidate_pattern(PoolName, Pattern) when is_binary(Pattern) ->
    Resp = eredis_pool:q(PoolName,
                         ["SCAN", 0, "MATCH", Pattern], ?TIMEOUT),
    case Resp of
        {ok, [NextCursor, NextKeys]} ->
            invalidate_pattern(PoolName, Pattern, NextCursor, NextKeys);
        {error, _}=E -> E
    end.

invalidate_pattern(PoolName, Prefix, Pattern) when is_binary(Prefix),
                                                   is_binary(Pattern) ->
    quintana:notify_spiral({?EREDIS_CACHE_FOLSOM_NAME(Prefix, <<"invalidate">>), 1}),
    Timer = quintana:begin_timed(?EREDIS_CACHE_FOLSOM_NAME(Prefix, <<"invalidate_latency">>)),
    FullPattern = <<Prefix/binary, Pattern/binary>>,
    Res = invalidate_pattern(PoolName, FullPattern),
    ok = quintana:notify_timed(Timer),
    Res.

invalidate_pattern(PoolName, _Pattern, <<"0">>, Keys) ->
    invalidate(PoolName, Keys);
invalidate_pattern(PoolName, Pattern, Cursor, Keys) ->
    case invalidate(PoolName, Keys) of
        ok ->
            Resp = eredis_pool:q(PoolName,
                                 ["SCAN", Cursor, "MATCH", Pattern], ?TIMEOUT),
            case Resp of
                {ok, [NextCursor, NextKeys]} ->
                    invalidate_pattern(PoolName, Pattern, NextCursor, NextKeys);
                {error, _}=E -> E
            end;
        {error, _}=E -> E
    end.
