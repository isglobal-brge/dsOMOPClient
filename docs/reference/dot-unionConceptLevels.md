# Merge per-server concept levels into one shared ordered spec

Pure reduction at the heart of the coordination layer: given each
server's `omopFactorLevelsDS` report, it computes, per concept-id
column, the union of admitted levels in one deterministic order. A
column flagged unsafe on any server refuses publication. Shared
assignment has no level-count cap; discovery was already checked at each
site. Numeric-looking IDs sort numerically; other labels use a
locale-independent radix sort.

## Usage

``` r
.unionConceptLevels(per_server)
```

## Arguments

  - per\_server:
    
    List of per-server results, each a list with `levels` (named list of
    column -\> character levels), `unsafe` (character vector of
    disclosive columns), and `nfilter_levels_max` (numeric server cap).
    `NULL` entries are rejected because a federation cannot be
    harmonized from partial results.

## Value

A named list mapping each harmonizable column to its shared, ordered
character levels. Empty list when nothing is harmonizable.

## Details

Kept side-effect-free (no DSI calls) so the union semantics are unit
testable in isolation.
