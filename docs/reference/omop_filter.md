# Create a filter specification

Filters restrict the population or events included in the extraction.
There are two executable levels: `"population"` (person-level inclusion
criteria) and `"row"` (event-level restrictions). Post-extraction
transformations belong in the output specification rather than in a
filter; the retired `"output"` filter level is rejected. Filters are
passed to
[`omop_recipe`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_recipe.md)
via its `filters` argument and can be nested into groups with
[`omop_filter_group`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_filter_group.md).

## Usage

``` r
omop_filter(
  type = c("sex", "age_range", "age_group", "cohort", "has_concept", "not_has_concept",
    "concept_count", "prior_observation", "followup", "visit_count", "has_measurement",
    "missing_measurement", "date_range", "concept_set", "value_bin", "value_concept",
    "custom"),
  level = c("population", "row"),
  params = list(),
  label = NULL
)

omop_filter_sex(value)

omop_filter_age(min = 0, max = 150, year = NULL, reference_date = NULL)

omop_filter_age_group(groups, year = NULL, reference_date = NULL)

omop_filter_cohort(cohort_definition_id)

omop_filter_has_concept(
  concept_id,
  table,
  concept_name = NULL,
  window = NULL,
  min_count = 1L,
  reference_date = NULL
)

omop_filter_date_range(start = NULL, end = NULL, date_column = NULL)

omop_filter_value(
  column = "value_as_number",
  threshold,
  direction = c("above", "below"),
  safe_bins = NULL
)

omop_filter_value_concept(
  concept_ids,
  column = "value_as_concept_id",
  concept_name = NULL
)

omop_filter_not_has_concept(
  concept_id,
  table,
  concept_name = NULL,
  window = NULL,
  reference_date = NULL
)

omop_filter_concept_count(
  concept_id,
  table,
  min_count = 2L,
  concept_name = NULL,
  window = NULL,
  reference_date = NULL
)

omop_filter_prior_observation(min_days = 365L, reference_date = NULL)

omop_filter_followup(min_days = 30L, reference_date = NULL)

omop_filter_visit_count(
  min_count = 1L,
  visit_concept_id = NULL,
  window = NULL,
  reference_date = NULL
)

omop_filter_has_measurement(
  concept_id,
  min_value = NULL,
  max_value = NULL,
  safe_bins = NULL,
  window = NULL,
  reference_date = NULL
)

omop_filter_missing_measurement(
  concept_id,
  window = NULL,
  reference_date = NULL
)
```

## Arguments

- type:

  Character; executable filter type. Population filters are `"sex"`,
  `"age_range"`, `"age_group"`, `"cohort"`, `"has_concept"`,
  `"not_has_concept"`, `"concept_count"`, `"prior_observation"`,
  `"followup"`, `"visit_count"`, `"has_measurement"`, and
  `"missing_measurement"`. Row filters are `"date_range"`,
  `"concept_set"`, `"value_bin"`, `"value_concept"`, and the fail-closed
  typed `"custom"` predicate.

- level:

  Character; `"population"` or `"row"`. When omitted, the unique
  executable level for `type` is selected. Output-level filters are not
  part of the executable Recipe contract.

- params:

  Named list; filter-specific parameters (varies by type).

- label:

  Character or `NULL`; human-readable description (auto-generated from
  type and params if `NULL`).

- value:

  Character; sex value. Accepts "F", "f", "female", "Female", "FEMALE",
  "M", "m", "male", "Male", "MALE" — normalized internally to "F" or
  "M".

- min:

  Numeric; minimum age (inclusive)

- max:

  Numeric; maximum age (inclusive)

- year:

  Integer or NULL; explicit calendar-year anchor (shorthand for July 1
  of that year). A cohort index supplies the anchor when omitted.

- reference_date:

  Date/string or `NULL`; fixed anchor for `window` when the population
  has no cohort index. An index is used when this is omitted.

- groups:

  Character vector; age group labels (e.g. c("18-24", "25-34"))

- cohort_definition_id:

  Integer; existing OMOP cohort_definition_id to require for membership.

- concept_id:

  Integer scalar or vector; measurement concept ID(s) to check absence
  of (a vector requires all of them to be absent)

- table:

  Character; which OMOP table to check

- concept_name:

  Character or NULL; human-readable name

- window:

  Named list with start/end index-relative day offsets, or NULL;
  restricts absence to that window (e.g. "no HbA1c in the prior year")

- min_count:

  Integer; minimum number of visits

- start:

  Character; inclusive start date in ISO `YYYY-MM-DD` form.

- end:

  Character; inclusive end date in ISO `YYYY-MM-DD` form and not before
  `start`. The server applies the authoritative minimum disclosure-safe
  width configured by the data controller.

- date_column:

  Character or `NULL`; explicit OMOP date column. When `NULL`,
  [`recipe_to_plan`](https://isglobal-brge.github.io/dsOMOPClient/reference/recipe_to_plan.md)
  infers the standard date column from the output's OMOP table.

- column:

  Character; the value-concept column (default `"value_as_concept_id"`)

- threshold:

  Numeric; threshold value

- direction:

  Character; "above" or "below"

- safe_bins:

  Server-issued result for the same measurement concept from
  [`ds.omop.safe.cutpoints()`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.safe.cutpoints.md).
  Required whenever a numeric range is supplied; client-authored
  thresholds are not executable.

- concept_ids:

  Integer scalar or vector; the value concept(s) to keep (a record
  matches if its value concept is any of them)

- min_days:

  Integer; minimum days of followup

- visit_concept_id:

  Integer scalar or vector, or NULL; visit type filter (a vector counts
  visits of any of the given types)

- min_value:

  Numeric or NULL; minimum value

- max_value:

  Numeric or NULL; maximum value

## Value

An `omop_filter` object (a named list with class `"omop_filter"`).

## Details

Convenience constructors are provided for common filter types:
`omop_filter_sex`, `omop_filter_age`, `omop_filter_age_group`,
`omop_filter_has_concept`, `omop_filter_date_range`,
`omop_filter_value`.

## See also

[`omop_recipe`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_recipe.md),
[`omop_filter_group`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_filter_group.md)

## Examples

``` r
if (FALSE) { # \dontrun{
f <- omop_filter(type = "sex", level = "population",
                 params = list(value = "F"))
} # }
```
