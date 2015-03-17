%%%-------------------------------------------------------------------
%%% @author Riccardo Massari <maxdrift85@gmail.com>
%%% @copyright (C) 2015 Riccardo Massari.
%%% @doc
%%% Erlang caching library based on Redis
%%% @end
%%%-------------------------------------------------------------------
-module(eredis_cache_app).

-behaviour(application).

-export([start/2, stop/1]).

%% Application callbacks

start(_StartType, _StartArgs) ->
    eredis_pool:start(),
    eredis_cache_sup:start_link().

stop(_State) ->
    ok.
