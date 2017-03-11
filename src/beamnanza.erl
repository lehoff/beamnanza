-module(beamnanza).

-type player_count() :: 3..7.
-type gold() :: non_neg_integer().

-export_type([player_count/0,
              gold/0]).
