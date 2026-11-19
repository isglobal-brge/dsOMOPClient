# Look up OMOP concepts by ID

Retrieves full concept metadata for one or more concept IDs from the
vocabulary tables on each connected server. This is useful for resolving
concept IDs obtained from clinical data tables back to their
human-readable names, domains, and vocabulary membership.

## Usage

``` r
ds.omop.concept.lookup(
  concept_ids,
  symbol = "omop",
  conns = NULL,
  execute = TRUE,
  type = "both",
  datasources = NULL
)
```

## Arguments

  - concept\_ids:
    
    Integer or numeric vector of OMOP concept IDs to look up (e.g.,
    `c(201826, 4329847)`).

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
available). Each server's result is a data frame with concept metadata
columns.

## Examples

``` r
if (FALSE) { # \dontrun{
# Look up a single concept
info <- ds.omop.concept.lookup(201826)
info$per_site

# Look up multiple concepts at once
info <- ds.omop.concept.lookup(c(201826, 4329847, 316139))
} # }
```
