# Release a pinned QueryLibrary sticky redesign

Verifies that every selected server advertises the same pinned semantic
redesign, then delegates to `ds.omop.dp.release`. Without `x`, the input
is prepared automatically from the redesign's `preparation` with
`ds.omop.dp.prepare` and removed after the release. Tables loaded with
`ds.omop.plan.execute` are refused by the server because the core never
uses DP. The upstream QueryLibrary SQL is never submitted to a server.
No client argument can control epsilon, seed, nonce, epoch, or rerolls.

## Usage

``` r
ds.omop.querylibrary.sticky.release(
  x = NULL,
  redesign,
  datasources = NULL,
  pool = TRUE,
  format = c("long", "wide", "vector", "raw"),
  type = NULL,
  symbol = "omop"
)
```

## Arguments

  - x:

    Bare DataSHIELD symbol naming a prepared person-local table, or
    `NULL` (the default) to prepare the input automatically from the
    recipe or plan stored in `redesign$preparation`: the release then
    runs `ds.omop.dp.prepare` into a temporary server symbol and removes
    it afterwards. A redesign may also be passed as the first argument.

  - redesign:

    An object created by `omop_querylibrary_sticky`.

  - datasources:

    Named DataSHIELD connection list. `NULL` uses active connections.

  - pool:

    Pool compatible releases across sites. For `bounded_distinct`,
    pooling is the sum of site-local cardinalities, not a cross-site set
    union; use `FALSE` for site-specific estimates.

  - format:

    Output format passed to `ds.omop.dp.release`.

  - type:

    Optional result view: `"split"`, `"combine"`, or `"both"`. When
    omitted, the legacy `pool` argument is preserved.

  - symbol:

    Session symbol of the OMOP connection, used only when the input is
    prepared automatically (default `"omop"`).

## Value

A `dsomop_result` from `ds.omop.dp.release`.
