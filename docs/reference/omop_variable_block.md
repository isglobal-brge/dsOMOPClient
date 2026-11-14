# Create a variable block

A variable block groups variables that share a source table, time
window, and row-level filters. When passed to `omop_recipe` via its
`blocks` argument, the block's `concept_ids` are expanded into
individual `omop_variable` objects that inherit the block's defaults.
This is the compact way to add many concepts from one table.

## Usage

``` r
omop_variable_block(
  id = NULL,
  table,
  concept_ids = integer(0),
  concept_names = NULL,
  time_window = NULL,
  format = "raw",
  value_source = NULL,
  suffix_mode = "index",
  filters = list(),
  population_id = "base",
  expand = FALSE,
  reference_date = NULL,
  unit = NULL,
  where = NULL,
  visit = NULL,
  concept_col = NULL,
  visit_filter = NULL
)
```

## Arguments

  - id:

    Character or `NULL`; block ID (auto-generated from table and concept
    count if `NULL`).

  - table:

    Character; shared source OMOP CDM table (e.g.
    `"condition_occurrence"`).

  - concept\_ids:

    Integer vector; concept IDs for all variables in the block.

  - concept\_names:

    Character vector or `NULL`; human-readable names matching
    `concept_ids` positionally.

  - time\_window:

    Named list with `start`/`end` offsets, or `NULL` for no window.

  - format:

    Character; default output format for variables in this block (e.g.
    `"binary"`, `"count"`).

  - value\_source:

    Character or `NULL`; default value source column (e.g.
    `"value_as_number"`).

  - suffix\_mode:

    Character; naming mode for multi-column expansion (`"index"`,
    `"range"`, or `"label"`).

  - filters:

    List of `omop_filter` objects; row-level filters applied to all
    variables in the block.

  - population\_id:

    Character; which population this block belongs to (default
    `"base"`).

  - expand:

    Logical; if `TRUE`, expand the block's concepts to include
    vocabulary descendants server-side (default `FALSE`).

  - reference\_date:

    Character/Date or `NULL`; fixed reference date required for a
    `"time_since"` block.

  - unit:

    Character or `NULL`; `"day"` (default for `time_since`) or complete
    calendar `"month"` units.

  - where:

    (2.10.3) A row predicate from `omop_where` applied to every variable
    of the block.

  - visit:

    (2.10.3) `NULL`, `list(type = <visit_concept_ids>)` or `"index"`,
    for every variable of the block.

  - concept\_col:

    (2.10.3) Concept column the block's concepts scope.

  - visit\_filter:

    (2.10.3) Legacy alias of `visit`, spelled as in `omop_variable`:
    `list(concept_ids = <visit_concept_ids>)` is `visit = list(type =
    <visit_concept_ids>)`. Giving both is an error.

## Value

An `omop_variable_block` object.

## See also

`omop_recipe`, `omop_variable`

## Examples

``` r
if (FALSE) { # \dontrun{
recipe <- omop_recipe(
  blocks = omop_variable_block(
    table = "condition_occurrence",
    concept_ids = c(201820, 320128),
    concept_names = c("Type 2 diabetes", "Essential hypertension"),
    format = "binary"
  ),
  outputs = omop_output(type = "wide")
)
} # }
```
