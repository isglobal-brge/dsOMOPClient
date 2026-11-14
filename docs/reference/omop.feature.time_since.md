# Create a time-since-event feature specification

Produces a feature spec that computes the elapsed time between the most
recent matching event and a reference date. The result is expressed in
the specified `unit` (days or months). Useful for calculating recency of
diagnoses, procedures, or measurements.

## Usage

``` r
omop.feature.time_since(
  concept_set,
  reference_date = NULL,
  unit = "day",
  name = NULL
)
```

## Arguments

  - concept\_set:

    Numeric vector of concept IDs, or an `omop_concept_set` object
    defining the concepts to match.

  - reference\_date:

    Character/Date; fixed ISO 8601 date used as the reference point. It
    is required: cohort-index recency is episode-specific and is not
    supported by the person-level feature reducer.

  - unit:

    Character; time unit for the result. One of `"day"` or `"month"`;
    months mean complete calendar months rather than fixed 30-day
    intervals.

  - name:

    Character; optional custom name for the feature column. If `NULL`,
    auto-generated from the concept and table context.

## Value

An `omop_feature_spec` object with `type = "time_since"`.

## See also

`ds.omop.plan.features`, `omop.feature.boolean`, `omop.feature.count`

## Examples

``` r
if (FALSE) { # \dontrun{
spec <- omop.feature.time_since(c(201826),
  reference_date = "2024-01-01", unit = "day")
plan <- ds.omop.plan.features(plan, "recency",
  "condition_occurrence",
  specs = list(days_since_diabetes = spec))
} # }
```
