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
  execute = TRUE
)
```

## Arguments

- concept_ids:

  Integer or numeric vector of concept IDs to fetch synonyms for (e.g.,
  `c(201826)`).

- symbol:

  Character; the session symbol used when the OMOP connection was
  initialised (default: `"omop"`).

- conns:

  DSI connection object(s). If `NULL` (the default), the connections
  stored in the active session are used.

- execute:

  Logical; if `FALSE`, returns a dry-run `dsomop_result` containing only
  the reproducible R code without contacting the servers.

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
