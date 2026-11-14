# Create a variable specification

Describes a single variable to extract from the CDM. Variables reference
a source table and column, and may include concept-level filtering and
output formatting options. Variables are the atomic building blocks
passed to `omop_recipe` via its `variables` argument (or grouped with
`omop_variable_block` and passed via `blocks`).

## Usage

``` r
omop_variable(
  name = NULL,
  table = NULL,
  column = NULL,
  concept_id = NULL,
  concept_name = NULL,
  type = c("auto", "numeric", "categorical", "date", "boolean", "integer", "character"),
  format = c("raw", "binary", "count", "first_value", "last_value", "mean", "min", "max",
    "time_since", "binned", "age", "sex_mf", "obs_duration", "drug_duration", "sum",
    "n_distinct", "sd", "cv", "slope", "abnormal_high", "abnormal_low", "gap_max",
    "gap_mean", "duration_sum", "prior_obs", "followup", "demo_missingness", "charlson",
    "chads2", "chadsvasc", "dcsi", "hfrs"),
  value_source = NULL,
  time_window = NULL,
  suffix_mode = c("index", "range", "label"),
  filters = list(),
  visit_filter = NULL,
  concept_col = NULL,
  expand = FALSE,
  reference_date = NULL,
  unit = NULL,
  event = NULL,
  reducer = NULL,
  where = NULL,
  visit = NULL,
  broadcast = FALSE,
  labels = NULL,
  column_names = NULL,
  date_spec = NULL
)
```

## Arguments

  - name:

    Character; output column name (auto-generated from `concept_name`,
    `concept_id`, or `column` if `NULL`).

  - table:

    Character; source OMOP CDM table (e.g. `"condition_occurrence"`).

  - column:

    Character or `NULL`; source column to extract.

  - concept\_id:

    Integer or `NULL`; concept ID filter (for concept columns).

  - concept\_name:

    Character or `NULL`; human-readable concept name.

  - type:

    Character; variable type hint. One of `"auto"`, `"numeric"`,
    `"categorical"`, `"date"`, `"boolean"`, `"integer"`, `"character"`.

  - format:

    Character; output format. One of `"raw"`, `"binary"`, `"count"`,
    `"first_value"`, `"last_value"`, `"mean"`, `"min"`, `"max"`, and
    `"time_since"`, plus the documented derived and longitudinal summary
    formats. `"time_since"` requires a fixed `reference_date`;
    cohort-index recency remains episode-specific and is rejected.
    Calendar binning is configured with `omop.date_handling`, not as a
    variable format.

  - value\_source:

    Character or `NULL`; column to extract value from (e.g.
    `"value_as_number"` for measurements).

  - time\_window:

    Named list with `start`/`end` offsets relative to index date, or
    `NULL` for no window constraint.

  - suffix\_mode:

    Character; how to name multi-column expansions (`"index"`,
    `"range"`, or `"label"`).

  - filters:

    List of `omop_filter` objects to apply to this variable.

  - visit\_filter:

    Named list `list(concept_ids = ...)` or `NULL`; restrict this
    variable's events to visits of those `visit_concept_id` values (via
    the `visit_occurrence_id` link).

  - concept\_col:

    Character or `NULL`; override the concept column the
    `concept_id`/concept set scopes (default: the table's domain
    concept), e.g. `"unit_concept_id"` to extract a single unit for
    harmonization.

  - expand:

    Logical; if `TRUE`, expand the concept to include vocabulary
    descendants server-side (default `FALSE`).

  - reference\_date:

    Character/Date or `NULL`; fixed ISO reference date required when
    `format = "time_since"`.

  - unit:

    Character or `NULL`; `"day"` (default for `time_since`) or
    `"month"`. Months are complete calendar months, not fixed 30-day
    intervals.

  - event:

    (2.10.3) An `omop_event` or `omop_era` (or a registry name): the
    language-2 spelling of the source. Give either `table`/`format` or
    `event`/`reducer`, never both.

  - reducer:

    (2.10.3) An `omop_reduce` reducer.

  - where:

    (2.10.3) A row predicate from `omop_where` that narrows this
    variable's rows.

  - visit:

    (2.10.3) `NULL`, `list(type = <visit_concept_ids>)` or `"index"`;
    `visit_filter` is kept as its legacy alias (giving both is an
    error).

  - broadcast:

    (2.10.3) Logical; allow this reducer column in a `long` output
    (broadcast per episode).

  - labels:

    Named character map from concept IDs to category labels.

  - column\_names:

    Named character map from stable untranslated generated column keys
    to complete, valid R names. Variable maps override recipe maps.

  - date\_spec:

    Native date presentation; variable fields override recipe defaults.

## Value

An `omop_variable` object (a named list with class `"omop_variable"`).

## See also

`omop_recipe`, `omop_variable_block`, `omop_reduce`

## Examples

``` r
if (FALSE) { # \dontrun{
recipe <- omop_recipe(
  variables = omop_variable(
    table = "condition_occurrence",
    concept_id = 201820,
    concept_name = "Type 2 diabetes",
    format = "binary"
  ),
  outputs = omop_output(type = "wide")
)
# Language 2: an event and a reducer
hba1c <- omop_event("measurement", 3004410, name = "hba1c")
omop_variable(name = "hba1c_last", event = hba1c,
              reducer = omop_reduce("last_value", window = omop_window(-365, 0)))
} # }
```
