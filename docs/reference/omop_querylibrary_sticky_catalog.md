# List audited sticky redesign mappings for OHDSI QueryLibrary

Lists semantic redesigns of questions in the pinned OHDSI QueryLibrary
snapshot. These are not ports of the upstream SQL. An executable mapping
fixes one of the seven person-bounded sticky primitives and a
longitudinal reducer for a protected table prepared from an
`omop_recipe` or `ds.omop.plan` with `ds.omop.dp.prepare` (or
automatically by `ds.omop.querylibrary.sticky.release`). With
`include_unavailable = TRUE`, the returned 201-ID partition also records
vocabulary/reference questions and blocked result shapes. Those rows are
not sticky-DP mappings and never authorize literal upstream SQL.

## Usage

``` r
omop_querylibrary_sticky_catalog(include_unavailable = FALSE)
```

## Arguments

  - include\_unavailable:

    Include vocabulary/reference metadata, explicitly held-back, and
    blocked upstream IDs as well as executable redesigns.

## Value

A data frame with mapping status, primitive family, statistic, reducer,
contribution contract, pinned source commit and `automatic_equivalent`:
the dsOMOP analysis-catalog entry (server 2.9.0 or later) that answers
the question with no preparation, or `NA`.

## Details

The catalog is not an arbitrary SQL or join gateway. It exposes no
analyst control over epsilon, seed, nonce, epoch, or rerolls. The
server-owned fixed per-release sticky contract governs every actual
release.
