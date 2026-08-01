# Define a server-owned sticky privacy release

Constructs the public semantic specification for a dedicated dsOMOP
sticky-noise release. Noise parameters and state are deliberately
absent: epsilon, seeds, nonces, privacy epochs, and ledger controls are
owned by each data custodian and cannot be supplied by the analyst. The
specification alone does not claim bounded lifetime privacy; that
depends on the server accounting mode reported by
[`ds.omop.dp.status()`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.dp.status.md).

## Usage

``` r
omop_privacy(
  statistic,
  variable = NULL,
  levels = NULL,
  breaks = NULL,
  lower = NULL,
  upper = NULL,
  reducer = "any",
  max_contributions = 1L,
  positive = NULL,
  order_by = NULL,
  denominator = c("all_persons", "nonmissing"),
  population_id = NULL
)
```

## Arguments

- statistic:

  One of `"count"`, `"categorical_histogram"`, `"numeric_histogram"`,
  `"bounded_mean"`, or `"binary_rate"`.

- variable:

  Bare column name for all statistics except `"count"`.

- levels:

  Public, fixed character domain for a categorical histogram. The server
  returns every requested level, including zero-count levels.

- breaks:

  Public, fixed, strictly increasing finite numbers, canonical
  `YYYY-MM-DD` dates, or `YYYY-MM-DDTHH:MM:SSZ` UTC datetimes.

- lower, upper:

  Public finite bounds for a bounded mean.

- reducer:

  Per-person reducer. Categorical histograms accept `"presence"`,
  `"mode"`, `"first"`, and `"last"`; numeric histograms also accept
  `"min"`, `"max"`, `"mean"`, `"median"`, and `"records"`; bounded means
  accept the numeric one-value reducers; binary rates accept `"any"`,
  `"all"`, `"first"`, and `"last"`. For compatibility, categorical
  `"any"` becomes `"presence"`, while numeric and bounded-mean `"any"`
  becomes `"mean"`.

- max_contributions:

  Positive integer person-level contribution cap. Values above one apply
  only to categorical histograms and numeric histograms reduced with
  `"presence"` or `"records"`.

- positive:

  Non-empty public value vector defining positive binary-rate records.
  It is canonicalized to sorted, unique character labels.

- order_by:

  Optional bare column defining longitudinal order. Required for
  `"first"` and `"last"`.

- denominator:

  Binary-rate denominator: all persons or only persons with a
  non-missing value.

- population_id:

  Optional public compatibility label for the population represented by
  `x`. When omitted,
  [`ds.omop.dp.release()`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.dp.release.md)
  derives it from the bare server symbol `x`. This label is metadata,
  not sticky release identity: changing it does not request or guarantee
  fresh noise. Do not put secrets or personal data in this public
  identifier.

## Value

A strictly validated `omop_privacy` specification.

## Details

Repeated longitudinal records are reduced or capped per person. Public
categorical levels are sorted canonically. Numeric histogram breaks may
be finite numbers, ISO dates, or canonical UTC datetimes. The `"first"`
and `"last"` reducers require an explicit public `order_by` column; row
order is never treated as longitudinal time.

## Examples

``` r
omop_privacy("count")
#> $statistic
#> [1] "count"
#>
#> $reducer
#> [1] "any"
#>
#> $max_contributions
#> [1] 1
#>
#> attr(,"class")
#> [1] "omop_privacy" "list"
omop_privacy("categorical_histogram", variable = "sex",
             levels = c("Female", "Male", "Unknown"))
#> $statistic
#> [1] "categorical_histogram"
#>
#> $variable
#> [1] "sex"
#>
#> $levels
#> [1] "Female"  "Male"    "Unknown"
#>
#> $reducer
#> [1] "presence"
#>
#> $max_contributions
#> [1] 1
#>
#> attr(,"class")
#> [1] "omop_privacy" "list"
omop_privacy("bounded_mean", variable = "value_as_number",
             lower = 0, upper = 300, reducer = "mean")
#> $statistic
#> [1] "bounded_mean"
#>
#> $variable
#> [1] "value_as_number"
#>
#> $lower
#> [1] 0
#>
#> $upper
#> [1] 300
#>
#> $reducer
#> [1] "mean"
#>
#> $max_contributions
#> [1] 1
#>
#> attr(,"class")
#> [1] "omop_privacy" "list"
```
