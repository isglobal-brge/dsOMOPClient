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
  execute = TRUE
)
```

## Arguments

- concept_ids:

  Integer or numeric vector of concept IDs to fetch relationships for
  (e.g., `c(201826)`).

- relationship_id:

  Character; optional single `relationship_id` filter (e.g. `"Maps to"`,
  `"Is a"`, `"Subsumes"`). `NULL` (the default) returns all relationship
  types.

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
