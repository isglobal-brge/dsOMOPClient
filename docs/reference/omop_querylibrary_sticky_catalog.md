# List audited sticky redesign mappings for OHDSI QueryLibrary

Lists semantic redesigns of questions in the pinned OHDSI QueryLibrary
snapshot. These are not ports of the upstream SQL. An executable mapping
fixes one of the seven person-bounded sticky primitives and a
longitudinal reducer for a protected table prepared through
[`omop_recipe`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_recipe.md)
or
[`ds.omop.plan`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.md).
With `include_unavailable = TRUE`, the returned 201-ID partition also
records vocabulary/reference questions and blocked result shapes. Those
rows are not sticky-DP mappings and never authorize literal upstream
SQL.

## Usage

``` r
omop_querylibrary_sticky_catalog(include_unavailable = FALSE)
```

## Arguments

- include_unavailable:

  Include vocabulary/reference metadata, explicitly held-back, and
  blocked upstream IDs as well as executable redesigns.

## Value

A data frame with mapping status, primitive family, statistic, reducer,
contribution contract and pinned source commit.

## Details

The catalog is not an arbitrary SQL or join gateway and does not certify
formal differential privacy. It exposes no `formal_dp` mode, epsilon,
seed, nonce, epoch, or reroll control. The server-owned sticky service
and its advertised accounting contract govern every actual release.
