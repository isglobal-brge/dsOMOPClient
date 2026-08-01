# Combine two cohorts with set operations

Combines two existing server-side cohort tables using a set operation
(intersection, union, or set difference). The result is assigned as a
new server-side symbol that can be used in subsequent queries or plan
executions.

## Usage

``` r
ds.omop.cohort.combine(
  op,
  cohort_a,
  cohort_b,
  new_name = NULL,
  symbol = "omop",
  conns = NULL
)
```

## Arguments

- op:

  Character; the set operation to apply. One of `"intersect"` (patients
  in both cohorts), `"union"` (patients in either cohort), or
  `"setdiff"` (patients in `cohort_a` but not `cohort_b`).

- cohort_a:

  Server-side cohort TABLE name for the first cohort (the value returned
  by
  [`ds.omop.cohort.create()`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.cohort.create.md)),
  its `dsomop_cohort_handle`, or a cohort definition ID (integer).

- cohort_b:

  Server-side cohort TABLE name for the second cohort (the value
  returned by
  [`ds.omop.cohort.create()`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.cohort.create.md)),
  its `dsomop_cohort_handle`, or a cohort definition ID (integer).

- new_name:

  Character; TABLE name for the combined result. If `NULL` (the
  default), an auto-generated name is used.

- symbol:

  Character; the session symbol used when the OMOP connection was
  initialised (default: `"omop"`).

- conns:

  DSI connection object(s). If `NULL` (the default), the connections
  stored in the active session are used.

## Value

Invisibly; a `dsomop_cohort_handle` carrying the server-side TABLE name
for the combined cohort. The handle can itself be passed as `cohort_a` /
`cohort_b` to a further `ds.omop.cohort.combine()`.

## Disclosure control

Each input is resolved + re-gated server-side, and the COMBINED result
is gated on its distinct-subject count: if an operand is unavailable
(absent/sub-threshold cohort_definition_id) or the combination yields
fewer than the server's per-subset threshold (`nfilter_subset`) persons,
the call FAILS CLOSED and no result table is materialised. An
"insufficient individuals" error here reflects the operands/operation
you chose and carries no disclosure about any pre-existing cohort.

## Examples

``` r
if (FALSE) { # \dontrun{
diabetes <- ds.omop.cohort.create(spec = ..., cohort_id = 1)
hypertension <- ds.omop.cohort.create(spec = ..., cohort_id = 2)
# Patients with both diabetes AND hypertension
combined <- ds.omop.cohort.combine(
  op = "intersect",
  cohort_a = diabetes,
  cohort_b = hypertension
)
} # }
```
