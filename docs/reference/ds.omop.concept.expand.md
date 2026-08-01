# Expand a concept set to a full list of concept IDs

Takes an `omop_concept_set` object (built with
[`ds.omop.concept.set`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.concept.set.md))
and resolves it on each connected server. Expansion applies descendant
inclusion, mapped-concept inclusion, and exclusion rules, returning the
final flat list of concept IDs that the set represents.

## Usage

``` r
ds.omop.concept.expand(
  concept_set,
  symbol = "omop",
  conns = NULL,
  execute = TRUE
)
```

## Arguments

- concept_set:

  An `omop_concept_set` object created by
  [`ds.omop.concept.set`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.concept.set.md).

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
available). Each server's result contains the resolved concept IDs.

## Examples

``` r
if (FALSE) { # \dontrun{
cs <- ds.omop.concept.set(c(201826), include_descendants = TRUE)
expanded <- ds.omop.concept.expand(cs)
expanded$per_site[[1]]
} # }
```
