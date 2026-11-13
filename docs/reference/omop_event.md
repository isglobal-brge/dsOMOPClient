# Define an event source

An event is a typed row source over one OMOP table: the rows of the
table (restricted to a concept set when one is given), narrowed by a row
predicate (`where`) and an optional visit restriction, with the numeric
value normalised by `unit` *before* any predicate or reducer reads it.

## Usage

``` r
omop_event(
  table,
  concepts = NULL,
  descendants = FALSE,
  mapped = FALSE,
  exclude = NULL,
  where = NULL,
  visit = NULL,
  value = NULL,
  unit = NULL,
  concept_col = NULL,
  end = NULL,
  name = NULL,
  on_missing_table = c("error", "unavailable")
)
```

## Arguments

  - table:

    OMOP table with a role record (any event table; not `cost`,
    `provider`, `location`, `care_site` or `episode_event`, which are
    reached through the bounded adapters `omop_cost` and
    `omop_episode_members`).

  - concepts:

    Concept IDs, an `omop_concept_set`, the name of a registry concept
    set, or `NULL` for every row of the table.

  - descendants, mapped, exclude:

    Expansion flags used when `concepts` is a vector of IDs (see
    `omop_concept_set`).

  - where:

    `NULL` or a row predicate from `omop_where` / `omop_where_tree` over
    the table's filterable columns and the normalised `value`.

  - visit:

    `NULL`, `list(type = <visit_concept_ids>)` or `"index"` (the visit
    of the index event).

  - value:

    Name of the numeric value column (default: the table's value role,
    `value_as_number` where present).

  - unit:

    `NULL` or an `omop_unit` conversion.

  - concept\_col:

    Scope the concepts on another concept column (for example
    `"unit_concept_id"`).

  - end:

    `NULL` (the table's end column), `"start"` (point events) or
    `list(fallback = "days_supply")`.

  - name:

    Event name (registry key; generated when `NULL`).

  - on\_missing\_table:

    `"error"` (default) or `"unavailable"`: a site without the table
    gets a typed unavailable state (criteria UNKNOWN, numeric and count
    features NA) instead of a failure.

## Value

An `omop_event` object.

## See also

`omop_where`, `omop_unit`, `omop_era`, `omop_variable`

## Examples

``` r
hba1c <- omop_event("measurement", 3004410,
  unit = omop_unit(8554, convert = list("8753" = c(factor = 0.0915, offset = 2.15))),
  where = omop_where(!is.na(value)), name = "hba1c")
```
