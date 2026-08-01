# Get a concept's ancestors and descendants (hierarchy)

Returns both the ancestors and descendants of one or more concept IDs
from the OMOP `concept_ancestor` table on each connected server, in a
single frame tagged with a `direction` column (`"ancestor"` or
`"descendant"`) and `levels_of_separation`. This is the Athena/ATLAS
hierarchy ("relationships" tree) view. Vocabulary reference data carries
no patient information, so this reader is not disclosure-gated and the
per-site frames are pooled by set union.

## Usage

``` r
ds.omop.concept.ancestors(
  concept_ids,
  symbol = "omop",
  conns = NULL,
  execute = TRUE
)
```

## Arguments

- concept_ids:

  Integer or numeric vector of concept IDs to expand the hierarchy for
  (e.g., `c(201826)`).

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

A `dsomop_result`. `per_site` holds each server's frame (with
`direction` and `levels_of_separation`); `pooled` is the de-duplicated
union across servers.

## Examples

``` r
if (FALSE) { # \dontrun{
tree <- ds.omop.concept.ancestors(201826)
tree$pooled                       # union across sites
subset(tree$pooled, direction == "ancestor")
} # }
```
