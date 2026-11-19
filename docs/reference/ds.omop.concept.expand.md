# Expand a concept set to a full list of concept IDs

Takes an `omop_concept_set` object (built with `ds.omop.concept.set`)
and resolves it on each connected server. Expansion applies descendant
inclusion, mapped-concept inclusion, and exclusion rules, returning the
final flat list of concept IDs that the set represents.

## Usage

``` r
ds.omop.concept.expand(
  concept_set,
  symbol = "omop",
  conns = NULL,
  execute = TRUE,
  type = "both",
  datasources = NULL
)
```

## Arguments

  - concept\_set:
    
    An `omop_concept_set` object created by `ds.omop.concept.set`.

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
available). Each server's result contains the resolved concept IDs.

## Examples

``` r
if (FALSE) { # \dontrun{
cs <- ds.omop.concept.set(c(201826), include_descendants = TRUE)
expanded <- ds.omop.concept.expand(cs)
expanded$per_site[[1]]
} # }
```
