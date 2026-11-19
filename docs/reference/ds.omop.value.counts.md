# Get value frequencies for a column

Returns the top-N most frequent distinct values for a column in an OMOP
CDM table, along with their counts. This is useful for profiling
categorical or low-cardinality columns such as `type_concept_id` or
`unit_concept_id`. Values below the disclosure threshold are left out,
surviving counts are banded, and concept names are resolved for concept
columns.

## Usage

``` r
ds.omop.value.counts(
  table,
  column,
  top_n = 20,
  concept_id = NULL,
  cohort = NULL,
  scope = c("per_site", "pooled"),
  pooling_policy = "strict",
  symbol = "omop",
  conns = NULL,
  execute = TRUE,
  type = "both",
  datasources = NULL
)
```

## Arguments

  - table:
    
    Character; the CDM table name (e.g., `"condition_occurrence"`).

  - column:
    
    Character; the column to count distinct values for (e.g.,
    `"condition_type_concept_id"`).

  - top\_n:
    
    Integer; the number of most frequent values to return (default: 20).

  - concept\_id:
    
    Integer or NULL; optional concept ID to restrict rows to a single
    concept of the table before counting values (e.g., the
    `value_as_concept_id` categories for one measurement concept).
    Default: NULL for all rows. The server applies the same disclosure
    controls to the concept-filtered population.

  - cohort:
    
    Cohort reference (a `dsomop_cohort_handle`, a
    `cohort_definition_id`, or a server-side cohort table name), or
    NULL.

  - scope:
    
    Deprecated alias of `type`: `"per_site"` means `type = "split"`,
    `"pooled"` means `type = "both"`.

  - pooling\_policy:
    
    Character; `"strict"` (default) or `"pooled_only_ok"`. See
    [dsomop\_conventions](https://isglobal-brge.github.io/dsOMOPClient/reference/dsomop_conventions.md).

  - symbol:
    
    Character; the session symbol (default: `"omop"`).

  - conns:
    
    Deprecated alias of `datasources`, kept for backward compatibility
    (a one-time message names the new argument).

  - execute:
    
    Logical; if `FALSE`, return a dry-run result containing only the
    generated call code (default: `TRUE`).

  - type:
    
    Result view: `"both"` (default), `"split"`, or `"combine"`. See
    [dsomop\_conventions](https://isglobal-brge.github.io/dsOMOPClient/reference/dsomop_conventions.md).

  - datasources:
    
    DataSHIELD connections (a named list of `DSConnection` objects).
    `NULL` (the default) uses the OMOP session's connections, or
    `DSI::datashield.connections_find()` when no session exists. See
    [dsomop\_conventions](https://isglobal-brge.github.io/dsOMOPClient/reference/dsomop_conventions.md).

## Value

A `dsomop_result`. Each `$per_site` entry is a data frame with `value`,
`n` (banded records), `n_persons` (banded distinct persons, person
tables) and `concept_name` (concept columns). `$pooled` sums `n` and
`n_persons` per value over the servers; a value must be released by
every server.

## Examples

``` r
if (FALSE) { # \dontrun{
vc <- ds.omop.value.counts("condition_occurrence",
                            "condition_type_concept_id",
                            top_n = 10)
vc$pooled
} # }
```
