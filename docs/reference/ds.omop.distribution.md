# Continuous-value distribution over a cohort, in one call

Thin wrapper over
[`ds.omop.analysis.run`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.analysis.run.md)
for the catalog's continuous-covariate analysis
(`"dsomop:fe.continuous"`): per-covariate count and
avg/sd/median/p10-p90 over a scoped cohort (measurement values, age, or
time-in-cohort). It builds the params and delegates, inheriting scoping,
pooling, optional plotting, and the ONE disclosure gate (which strips
min/max and masks sub-threshold stats) unchanged.

## Usage

``` r
ds.omop.distribution(
  cohort = NULL,
  metric = "measurement_value",
  domain = "measurement",
  top_n = 50,
  concept_id = NULL,
  tables = NULL,
  plot = FALSE,
  symbol = "omop",
  conns = NULL
)
```

## Arguments

- cohort:

  Cohort reference to scope to (handle, `cohort_definition_id`, or
  server-side table name). Required unless `tables` is given.

- metric:

  Character; `"measurement_value"` (default), `"age"`, or
  `"time_in_cohort"`.

- domain:

  Character; value domain for `metric = "measurement_value"` by name
  (`"measurement"` / `"observation"`) or code (`"3"`/`"4"`). Default
  `"measurement"`.

- top_n:

  Integer; number of top covariates to return (default 50).

- concept_id:

  Integer vector or `NULL`; narrow the gated result to these covariate
  concept id(s) (post-gate row subset). Default `NULL`.

- tables:

  Optional character vector of `omop.table` symbol names to scope to;
  may be combined with `cohort`.

- plot:

  Logical; build the entry's client-side plot (default `FALSE`).

- symbol:

  Character; the session symbol (default `"omop"`).

- conns:

  DSI connection object(s) or `NULL` to use the session default.

## Value

A `dsomop_result` (see
[`ds.omop.analysis.run`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.analysis.run.md)).

## Details

As with
[`ds.omop.prevalence`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.prevalence.md),
the cohort IS the population, so a `cohort`/`tables` scope is required;
an un-scoped call errors clearly.

## See also

[`ds.omop.analysis.run`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.analysis.run.md),
[`ds.omop.prevalence`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.prevalence.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Measurement-value distributions over a cohort, one call.
ds.omop.distribution(cohort = my_cohort)

# Age distribution of the cohort.
ds.omop.distribution(cohort = my_cohort, metric = "age")
} # }
```
