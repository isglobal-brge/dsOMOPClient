# Get descendant concepts via the concept\_ancestor table

Traverses the OMOP `concept_ancestor` hierarchy on each connected server
and returns all descendant concepts for the given ancestor IDs. This is
the standard way to expand a high-level concept (e.g., "Diabetes
mellitus") into all of its more specific child concepts.

## Usage

``` r
ds.omop.concept.descendants(
  ancestor_ids,
  include_self = TRUE,
  symbol = "omop",
  conns = NULL,
  execute = TRUE,
  type = "both",
  datasources = NULL
)
```

## Arguments

  - ancestor\_ids:
    
    Integer or numeric vector of ancestor concept IDs to expand (e.g.,
    `c(201820)`).

  - include\_self:
    
    Logical; if `TRUE` (the default), the ancestor concepts themselves
    are included in the result alongside their descendants.

  - symbol:
    
    Character; the session symbol used when the OMOP connection was
    initialised (default: `"omop"`).

  - conns:
    
    Deprecated alias of `datasources`, kept for backward compatibility
    (a one-time message names the new argument).

  - execute:
    
    Logical; if `FALSE`, returns a dry-run `dsomop_result` containing
    only the reproducible R code without contacting the servers.

  - type:
    
    Result view: `"both"` (default), `"split"`, or `"combine"`. The
    pooled view is the de-duplicated union of the servers' rows (shared
    reference data, no patient counts); it is `NULL` with
    `meta$pooling_reason` when a server did not answer. See
    [dsomop\_conventions](https://isglobal-brge.github.io/dsOMOPClient/reference/dsomop_conventions.md).

  - datasources:
    
    DataSHIELD connections (a named list of `DSConnection` objects).
    `NULL` (the default) uses the OMOP session's connections, or
    `DSI::datashield.connections_find()` when no session exists. See
    [dsomop\_conventions](https://isglobal-brge.github.io/dsOMOPClient/reference/dsomop_conventions.md).

## Value

A `dsomop_result` object with `scope = "pooled"` (a de-duplicated
cross-site view of the shared vocabulary; per-site frames remain
available). Each server's result is a data frame of descendant concept
rows.

## Examples

``` r
if (FALSE) { # \dontrun{
# Get all descendants of "Type 2 diabetes mellitus" (concept 201826)
desc <- ds.omop.concept.descendants(201826)
nrow(desc$per_site[[1]])

# Exclude the ancestor itself
desc <- ds.omop.concept.descendants(201826, include_self = FALSE)
} # }
```
