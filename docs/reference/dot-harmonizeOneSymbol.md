# Harmonize the concept-id columns of one server-side symbol

Implements the three-phase coordination for a single symbol: (1)
aggregate each server's safe levels via `omopFactorLevelsDS`; (2) union
them client-side into one deterministic ordering (numeric ids sorted
numerically, character names sorted lexically), dropping any column
flagged unsafe on *any* server and any union exceeding the smallest
server cap; (3) broadcast the shared spec back via
`omopAsFactorColumnsDS`.

## Usage

``` r
.harmonizeOneSymbol(sym, conns)
```

## Arguments

- sym:

  Character; the server-side symbol to harmonize.

- conns:

  DSI connections object restricted to servers holding `sym`.

## Value

`NULL` invisibly.
