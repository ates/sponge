-define(INFO(Format), lager:log(info, self(), Format, [])).
-define(INFO(Format, Args), lager:log(info, self(), Format, Args)).

-define(ERROR(Format), lager:log(error, self(), Format, [])).
-define(ERROR(Format, Args), lager:log(error, self(), Format, Args)).

-define(DEBUG(Format), lager:log(debug, self(), Format, [])).
-define(DEBUG(Format, Args), lager:log(debug, self(), Format, Args)).

-define(DEFAULT_TTL, 10000). % 10 seconds

-define(TABLE, sponge_warehouse).

-record(sponge_warehouse, {
    key :: term(),
    value :: term(),
    ttl = ?DEFAULT_TTL :: pos_integer()
}).
