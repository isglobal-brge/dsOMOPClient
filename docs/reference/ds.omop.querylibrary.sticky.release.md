# Release a pinned QueryLibrary sticky redesign

Verifies that every selected server advertises the same pinned semantic
redesign, then delegates to
[`ds.omop.dp.release`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.dp.release.md).
`x` must be the single memory-mode output assigned by the Recipe/Plan
preparation. The upstream QueryLibrary SQL is never submitted to a
server. No client argument can select a separate formal-DP mode or
control epsilon, seed, nonce, epoch, or rerolls.

## Usage

``` r
ds.omop.querylibrary.sticky.release(
  x,
  redesign,
  datasources = NULL,
  pool = TRUE,
  format = c("long", "wide", "vector", "raw")
)
```

## Arguments

- x:

  Bare DataSHIELD symbol naming the prepared person-local table.

- redesign:

  An object created by
  [`omop_querylibrary_sticky`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_querylibrary_sticky.md).

- datasources:

  Named DataSHIELD connection list. `NULL` uses active connections.

- pool:

  Pool compatible releases across sites. For `bounded_distinct`, pooling
  is the sum of site-local cardinalities, not a cross-site set union;
  use `FALSE` for site-specific estimates.

- format:

  Output format passed to
  [`ds.omop.dp.release`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.dp.release.md).

## Value

A `dsomop_result` from
[`ds.omop.dp.release`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.dp.release.md).
