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
         get/3,
         set/3,
         set/4,
         set/5,
         get_keys/2,
         get_keys/3,
         invalidate/2,
         invalidate/3,
         invalidate_pattern/2,
         invalidate_pattern/3,
         invalidate_pattern/4
        ]).

start_cache(CacheName, PoolArgs, WorkerArgs) ->
    eredis_pool_sup:create_pool(CacheName, PoolArgs, WorkerArgs).

stop_cache(CacheName) ->
    eredis_pool_sup:delete_pool(CacheName).

get(PoolName, Key) when is_binary(Key) ->
    get(PoolName, Key, ?TIMEOUT).

get(PoolName, Key, Timeout) when is_binary(Key),
                                 is_integer(Timeout) ->
    case eredis_pool:q(PoolName, ["GET", Key], Timeout) of
        {ok, undefined}=R -> R;
        {ok, Value} -> {ok, binary_to_term(Value)};
        {error, _}=E -> E
    end.

set(PoolName, Key, Value) when is_binary(Key) ->
    set(PoolName, Key, Value, ?DEF_VALIDITY).

set(PoolName, Key, Value, Validity) when is_binary(Key),
                                         is_integer(Validity) ->
    set(PoolName, Key, Value, Validity, ?TIMEOUT).

set(PoolName, Key, Value, Validity, Timeout) when is_binary(Key),
                                                  is_integer(Validity),
                                                  is_integer(Timeout) ->
    BinValue = term_to_binary(Value),
    Fun = fun(Worker) ->
                  case eredis:q(Worker, ["SET", Key, BinValue], Timeout) of
                      {ok, <<"QUEUED">>} ->
                          case eredis:q(Worker, ["EXPIRE", Key, Validity], Timeout) of
                              {ok, <<"QUEUED">>} -> ok;
                              {error, _}=E -> E
                          end;
                      {error, _}=E -> E
                  end
          end,
    case eredis_pool:transaction(PoolName, Fun, Timeout) of
        {ok, [<<"OK">>, <<"1">>]} -> ok;
        {error, _}=E -> E
    end.

get_keys(PoolName, Pattern) when is_binary(Pattern) ->
    get_keys(PoolName, Pattern, ?TIMEOUT).

get_keys(PoolName, Pattern, Timeout) when is_binary(Pattern),
                                          is_integer(Timeout) ->
    eredis_pool:q(PoolName, ["KEYS", Pattern], Timeout).

invalidate(PoolName, Key) ->
    invalidate(PoolName, Key, ?TIMEOUT).

invalidate(_PoolName, [], _) ->
    ok;
invalidate(PoolName, [Key | Rest], Timeout) ->
    case invalidate(PoolName, Key, Timeout) of
        ok ->
            invalidate(PoolName, Rest, Timeout);
        {error, _}=E -> E
    end;
invalidate(PoolName, Key, Timeout) when is_binary(Key),
                                        is_integer(Timeout) ->
    case eredis_pool:q(PoolName, ["DEL", Key], Timeout) of
        {ok, _} -> ok;
        {error, _}=E -> E
    end.

invalidate_pattern(PoolName, Prefix) ->
    invalidate_pattern(PoolName, Prefix, ?TIMEOUT).

invalidate_pattern(PoolName, Prefix, Pattern) when is_binary(Prefix),
                                                   is_binary(Pattern) ->
    invalidate_pattern(PoolName, Prefix, Pattern, ?TIMEOUT);
invalidate_pattern(PoolName, Pattern, Timeout) when is_binary(Pattern),
                                                    is_integer(Timeout) ->
    Resp = eredis_pool:q(PoolName,
                         ["SCAN", 0, "MATCH", Pattern], Timeout),
    case Resp of
        {ok, [NextCursor, NextKeys]} ->
            invalidate_pattern(PoolName, Pattern, NextCursor, NextKeys, Timeout);
        {error, _}=E -> E
    end.

invalidate_pattern(PoolName, Prefix, Pattern, Timeout) when is_binary(Prefix),
                                                            is_binary(Pattern),
                                                            is_integer(Timeout) ->
    quintana:notify_spiral({?EREDIS_CACHE_FOLSOM_NAME(Prefix, <<"invalidate">>), 1}),
    Timer = quintana:begin_timed(?EREDIS_CACHE_FOLSOM_NAME(Prefix, <<"invalidate_latency">>)),
    FullPattern = <<Prefix/binary, Pattern/binary>>,
    Res = invalidate_pattern(PoolName, FullPattern, Timeout),
    ok = quintana:notify_timed(Timer),
    Res.

invalidate_pattern(PoolName, _Pattern, <<"0">>, Keys, Timeout) ->
    invalidate(PoolName, Keys, Timeout);
invalidate_pattern(PoolName, Pattern, Cursor, Keys, Timeout) ->
    case invalidate(PoolName, Keys, Timeout) of
        ok ->
            Resp = eredis_pool:q(PoolName,
                                 ["SCAN", Cursor, "MATCH", Pattern], Timeout),
            case Resp of
                {ok, [NextCursor, NextKeys]} ->
                    invalidate_pattern(PoolName, Pattern, NextCursor, NextKeys, Timeout);
                {error, _}=E -> E
            end;
        {error, _}=E -> E
    end.
