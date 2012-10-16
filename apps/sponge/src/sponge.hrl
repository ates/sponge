-define(INFO(Format, Args), error_logger:info_msg(Format, Args)).
-define(ERROR(Format, Args), error_logger:error_msg(Format, Args)).

-define(DEFAULT_TTL, 10000). % 10 seconds

-define(TABLE, sponge_warehouse).

-record(sponge_warehouse, {
    key :: term(),
    value :: term(),
    ttl = ?DEFAULT_TTL :: pos_integer()
}).
