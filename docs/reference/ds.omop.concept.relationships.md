# Get the relationships of one or more concepts

Returns every `concept_relationship` edge touching the given concept IDs
on each connected server, in **both** directions (the related concept's
name is joined in and a `direction` column distinguishes `"forward"`
from `"reverse"`). An optional `relationship_id` narrows the result to a
single relationship type. Vocabulary reference data carries no patient
information, so this reader is not disclosure-gated and the per-site
frames are pooled by set union.

## Usage

``` r
ds.omop.concept.relationships(
  concept_ids,
  relationship_id = NULL,
  symbol = "omop",
  conns = NULL,
  execute = TRUE,
  type = "both",
  datasources = NULL
)
```

## Arguments

  - concept\_ids:
    
    Integer or numeric vector of concept IDs to fetch relationships for
    (e.g., `c(201826)`).

  - relationship\_id:
    
    Character; optional single `relationship_id` filter (e.g. `"Maps
    to"`, `"Is a"`, `"Subsumes"`). `NULL` (the default) returns all
    relationship types.

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

A `dsomop_result`. `per_site` holds each server's relationship frame
(with a `direction` column); `pooled` is the de-duplicated union across
servers.

## Examples

``` r
if (FALSE) { # \dontrun{
# All relationships of a concept
rels <- ds.omop.concept.relationships(201826)
rels$pooled

# Only the "Maps to" edges
maps <- ds.omop.concept.relationships(201826, relationship_id = "Maps to")
} # }
```
