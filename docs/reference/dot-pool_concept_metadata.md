# Pool vocabulary / concept METADATA rows across servers (de-duplicated union)

Concept metadata (ids, names, domains, descendants, expansions) is
shared vocabulary: identical across sites and non-disclosive (it carries
no patient counts), so the pooled view is the row-wise union
de-duplicated to a single clean answer. Per-site frames remain available
on the result for inspecting any cross-site vocabulary differences. A
partial server response returns NULL instead of presenting an incomplete
union as federation-wide. Returns NULL when no site had data.

## Usage

``` r
.pool_concept_metadata(per_site)
```

## Arguments

- per_site:

  Named list of per-server data frames.

## Value

A de-duplicated data frame, or NULL.
