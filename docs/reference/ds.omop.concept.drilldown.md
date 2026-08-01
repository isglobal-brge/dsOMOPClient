# Get concept drilldown profile

Returns a comprehensive drilldown profile for a single concept within an
OMOP CDM table. The profile includes summary statistics (record count,
person count), numeric distribution (if applicable), categorical value
breakdown, date coverage range, and missingness rates for associated
columns. All results are disclosure-controlled on the server side before
being returned.

## Usage

``` r
ds.omop.concept.drilldown(
  table,
  concept_id,
  concept_col = NULL,
  scope = c("per_site", "pooled"),
  pooling_policy = "strict",
  symbol = "omop",
  conns = NULL,
  execute = TRUE
)
```

## Arguments

- table:

  Character; the CDM table name (e.g., `"condition_occurrence"`,
  `"measurement"`).

- concept_id:

  Integer; the OMOP concept ID to drill into.

- concept_col:

  Character; the concept column to drill into, or NULL for automatic
  detection based on the table's standard concept column (default:
  NULL).

- scope:

  Character; `"per_site"` (default) or `"pooled"`.

- pooling_policy:

  Character; `"strict"` (default) or `"pooled_only_ok"`.

- symbol:

  Character; the session symbol (default: `"omop"`).

- conns:

  DSI connection object(s) or NULL to use the session default.

- execute:

  Logical; if `FALSE`, return a dry-run result containing only the
  generated call code (default: `TRUE`).

## Value

A `dsomop_result` object with `$per_site` (named list of lists
containing `summary`, `numeric`, `categorical`, `date_range`, and
`missingness` components), `$pooled` (combined profile when pooled), and
`$meta`.

## Examples

``` r
if (FALSE) { # \dontrun{
profile <- ds.omop.concept.drilldown("condition_occurrence",
                                      concept_id = 201820)
profile$per_site$server1$summary
profile$per_site$server1$numeric
} # }
```
