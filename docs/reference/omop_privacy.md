# Define a server-owned sticky privacy release

Constructs the public semantic specification for a dedicated dsOMOP
sticky-noise release. Noise parameters and state are deliberately
absent: epsilon, seeds, nonces, and privacy epochs are owned by each
data custodian and cannot be supplied by the analyst. On dsOMOP 2.10.3,
supported views share canonical protected components. A fixed synopsis
declares its bounded-vector epsilon before exact support gates; a
realised contribution release is heuristic and claims no mechanism
epsilon or delta. Neither route claims differential privacy for the
whole API transcript. Servers 2.10.0 to 2.10.2 instead derive each
answer from the server's secret root, the question, the protected
statistic and a private automatic database version: valid repeats return
the same answer, and a database reload that adds rows re-draws every
answer. Servers 2.7.1 to 2.9.x instead retain the first complete answer
per public request, snapshot and privacy epoch. There is no lifetime
privacy budget or call counter.

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

    One of `"count"`, `"bounded_record_count"`,
    `"categorical_histogram"`, `"numeric_histogram"`,
    `"bounded_distinct"`, `"bounded_mean"`, or `"binary_rate"`.

  - variable:

    Bare column name except for `"count"` and `"bounded_record_count"`.

  - levels:

    Public character labels for a categorical histogram. On 2.10.3 these
    must be a view of the server's complete registered domain. The
    server returns every requested level, including zero-count levels.

  - breaks:

    Public, fixed, strictly increasing finite numbers, canonical
    `YYYY-MM-DD` dates, or `YYYY-MM-DDTHH:MM:SSZ` UTC datetimes.

  - lower, upper:

    Public finite bounds for a bounded mean.

  - reducer:

    Per-person reducer. Categorical histograms accept `"presence"`,
    `"mode"`, `"first"`, `"last"`, and `"records"`; numeric histograms
    also accept `"min"`, `"max"`, `"mean"`, `"median"`, and `"records"`;
    bounded means accept the numeric one-value reducers; binary rates
    accept `"any"`, `"all"`, `"first"`, and `"last"`. For compatibility,
    categorical `"any"` becomes `"presence"`, numeric and bounded-mean
    `"any"` becomes `"mean"`, and the two scalar bounded primitives
    canonicalize it to their sole reducer.

  - max\_contributions:

    Positive integer person-level contribution cap. Values above one
    apply to bounded record counts, bounded distinct counts, categorical
    `"presence"`/`"records"` histograms, and numeric `"records"`
    histograms.

  - positive:

    Non-empty public value vector defining positive binary-rate records.
    It is canonicalized to sorted, unique character labels.

  - order\_by:

    Optional bare column defining longitudinal order. Required for
    `"first"` and `"last"`.

  - denominator:

    Binary-rate denominator: all persons or only persons with a
    non-missing value.

  - population\_id:

    Optional public compatibility label for the population represented
    by `x`. When omitted, `ds.omop.dp.release()` derives it from the
    bare server symbol `x`. This label is metadata, not sticky release
    identity: changing it does not request or guarantee fresh noise. Do
    not put secrets or personal data in this public identifier.

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
omop_privacy("categorical_histogram", variable = "sex",
             levels = c("Female", "Male", "Unknown"))
omop_privacy("bounded_mean", variable = "value_as_number",
             lower = 0, upper = 300, reducer = "mean")
```
