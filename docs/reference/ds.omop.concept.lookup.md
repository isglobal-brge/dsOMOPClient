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
  execute = TRUE
)
```

## Arguments

- concept_ids:

  Integer or numeric vector of OMOP concept IDs to look up (e.g.,
  `c(201826, 4329847)`).

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
