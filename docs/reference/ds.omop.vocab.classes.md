# List the concept classes available on each server

Returns the distinct concept classes from the OMOP `concept_class` table
on each connected server (falling back to distinct `concept_class_id`
values on `concept` when the concept_class table is not loaded).
Vocabulary reference data carries no patient information, so this reader
is not disclosure-gated and the per-site frames are pooled by set union.

## Usage

``` r
ds.omop.vocab.classes(symbol = "omop", conns = NULL, execute = TRUE)
```

## Arguments

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

A `dsomop_result`. `per_site` holds each server's concept class frame;
`pooled` is the de-duplicated union across servers.

## Examples

``` r
if (FALSE) { # \dontrun{
classes <- ds.omop.vocab.classes()
classes$pooled
} # }
```
