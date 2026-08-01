# Union and de-duplicate per-site vocabulary data frames

Vocabulary/CDM metadata is reference data: a given concept, vocabulary,
domain, or concept class exists independently on each server, so the
cross- site view is simply their set union (not a sum). This helper
row-binds the per-site data frames (intersecting on common columns so
heterogeneous schemas still combine) and drops duplicate rows. Empty /
non-data-frame entries are skipped. A partial server response returns
`NULL` rather than claiming an incomplete union is federation-wide.
Returns `NULL` when there is nothing to pool.

## Usage

``` r
.pool_vocab_union(per_site)
```

## Arguments

- per_site:

  Named list of per-server data frames.

## Value

A single de-duplicated data frame, or `NULL`.
