-define(DEF_VALIDITY, 60).
-define(DEF_ERR_VALIDITY, 0).

-compile([{parse_transform, decorator_pt_core}]).

%% Parse transform for the ?EREDIS_CACHE decorator
-define(EREDIS_CACHE(PoolName, Options),
        -decorate({eredis_cache_decorator, eredis_cache_pt,
                   {?MODULE, ?FUNCTION, PoolName, Options}})).