# Browse the concept catalog with pagination

Pages through the OMOP `concept` catalog on each connected server,
filtered by domain, vocabulary, concept class, standard status and
validity, using OFFSET/LIMIT pagination (page size capped server-side at
1000). Unlike
[`ds.omop.concept.search`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.concept.search.md)
this returns the total matching count alongside the current page so
callers can build pagination controls. Vocabulary reference data carries
no patient information, so this reader is not disclosure-gated; the
per-site pages are pooled by set union and the per-site `total_count`
values are summed.

## Usage

``` r
ds.omop.concept.list(
  domain = NULL,
  vocabulary = NULL,
  concept_class = NULL,
  standard = NULL,
  valid = NULL,
  offset = 0,
  limit = 100,
  order = NULL,
  symbol = "omop",
  conns = NULL,
  execute = TRUE
)
```

## Arguments

- domain:

  Character; filter by `domain_id` (e.g. `"Condition"`). `NULL` (the
  default) applies no domain filter.

- vocabulary:

  Character; filter by `vocabulary_id` (e.g. `"SNOMED"`). `NULL` (the
  default) applies no vocabulary filter.

- concept_class:

  Character; filter by `concept_class_id`. `NULL` (the default) applies
  no class filter.

- standard:

  Character; filter by `standard_concept` value (e.g. `"S"`). `NULL`
  (the default) applies no standard filter.

- valid:

  Logical; `TRUE` keeps only currently-valid concepts, `FALSE` only
  invalidated ones. `NULL` (the default) returns both.

- offset:

  Integer; number of rows to skip (page start). Default `0`.

- limit:

  Integer; page size (default `100`; capped at 1000 server-side).

- order:

  Character; column to order by. `NULL` (the default) uses the server
  default (`concept_id`).

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

A `dsomop_result`. `per_site` holds each server's
`list(rows, total_count, offset, limit)`; `pooled` is a list with `rows`
(the de-duplicated union of the page rows) and `total_count` (the summed
per-site totals).

## Examples

``` r
if (FALSE) { # \dontrun{
# First page of SNOMED conditions, with the total available count
page <- ds.omop.concept.list(domain = "Condition", vocabulary = "SNOMED",
                             limit = 100)
page$pooled$total_count
nrow(page$pooled$rows)

# Next page
page2 <- ds.omop.concept.list(domain = "Condition", vocabulary = "SNOMED",
                              offset = 100, limit = 100)
} # }
```
