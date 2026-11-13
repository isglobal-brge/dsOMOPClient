# Prepare a person-local table for sticky DP releases

Runs a typed `omop_recipe` or `omop_plan` on every server through the
community-tool method `omopDpPlanExecuteDS` (dsOMOP 2.9.1 or later),
which builds the tables with the same plan engine as
`ds.omop.plan.execute` and seals them with the authenticated DP
provenance that `ds.omop.dp.release` requires. Ordinary table loading
never involves DP; use this function only to define the population and
the derived variables of a DP release (for example an age at a public
reference year). The analyst chooses that population; the data custodian
owns every privacy setting (epsilon, caps, whether DP is available).

## Usage

``` r
ds.omop.dp.prepare(x, out = NULL, symbol = "omop", conns = NULL)
```

## Arguments

  - x:

    An `omop_recipe` or `omop_plan`.

  - out:

    Output symbols, as in `recipe_execute` and `ds.omop.plan.execute`.

  - symbol:

    Character; the session symbol used when the OMOP connection was
    initialised (default `"omop"`).

  - conns:

    DSI connection object(s). If `NULL` (the default), the connections
    stored in the active session are used.

## Value

The named output-to-symbol mapping, invisibly.

## Details

Prepared tables keep their concept identifiers as stored (no
concept-factor harmonization), and filtering, merging or otherwise
manipulating them drops the DP provenance: define the whole release
population in the recipe.

## Examples

``` r
if (FALSE) { # \dontrun{
recipe <- omop_recipe(
  variables = omop_variable_age(name = "age_at_2025", year = 2025),
  outputs = omop_output(name = "ages", type = "wide")
)
ds.omop.dp.prepare(recipe, out = c(ages = "DP_ages"))
ds.omop.dp.release("DP_ages", omop_privacy(
  "numeric_histogram", variable = "age_at_2025",
  breaks = seq(0, 120, by = 10), reducer = "mean"))
} # }
```
