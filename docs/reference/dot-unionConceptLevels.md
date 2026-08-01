# Merge per-server concept levels into one shared ordered spec

Pure reduction at the heart of the coordination layer: given each
server's `omopFactorLevelsDS` report, it computes, per concept-id
column, the union of safe levels in one deterministic order. A column
flagged unsafe on *any* server is dropped entirely (left raw
everywhere), and a union exceeding the smallest reported server cap is
dropped (no server would accept it). Numeric-looking ids sort
numerically so the shared coding is intuitive; other labels use a
locale-independent radix sort so every client derives the identical
ordering.

## Usage

``` r
.unionConceptLevels(per_server)
```

## Arguments

- per_server:

  List of per-server results, each a list with `levels` (named list of
  column -\> character levels), `unsafe` (character vector of disclosive
  columns), and `nfilter_levels_max` (numeric server cap). `NULL`
  entries are rejected because a federation cannot be harmonized from
  partial results.

## Value

A named list mapping each harmonizable column to its shared, ordered
character levels. Empty list when nothing is harmonizable.

## Details

Kept side-effect-free (no DSI calls) so the union semantics are unit
testable in isolation.
