# Get the synonyms of one or more concepts

Returns the alternative names for one or more concept IDs from the OMOP
`concept_synonym` table on each connected server, mirroring the Athena
concept "synonyms" panel. Vocabulary reference data carries no patient
information, so this reader is not disclosure-gated and the per-site
frames are pooled by set union.

## Usage

``` r
ds.omop.concept.synonyms(
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
    
    Integer or numeric vector of concept IDs to fetch synonyms for
    (e.g., `c(201826)`).

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

A `dsomop_result`. `per_site` holds each server's `concept_id` /
`concept_synonym_name` frame; `pooled` is the de-duplicated union across
servers.

## Examples

``` r
if (FALSE) { # \dontrun{
syns <- ds.omop.concept.synonyms(201826)
syns$pooled
} # }
```
