# Subset an already-gated result's frames to requested concept id(s)

Post-gate, cosmetic row selection: keeps only the rows whose
covariate/concept id is in `concept_id`, in BOTH the pooled and per-site
frames. This is a plain subset of numbers that already cleared the
server's disclosure gate — it is NOT a new gate and never recovers a
suppressed cell. Frames without an id column are returned untouched
(defensive).

## Usage

``` r
.analysis_filter_concepts(result, concept_id = NULL)
```

## Arguments

- result:

  A `dsomop_result`.

- concept_id:

  Integer vector of concept ids to keep, or `NULL`.

## Value

The `dsomop_result` with its frames row-subset.
