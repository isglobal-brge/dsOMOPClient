# List the vocabularies available on each server

Returns the distinct vocabularies from the OMOP `vocabulary` table on
each connected server (falling back to distinct `vocabulary_id` values
on `concept` when the vocabulary table is not loaded). Vocabulary
reference data carries no patient information, so this reader is not
disclosure-gated and the per-site frames are pooled by set union (a
vocabulary may exist on several servers).

## Usage

``` r
ds.omop.vocab.vocabularies(symbol = "omop", conns = NULL, execute = TRUE)
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

A `dsomop_result`. `per_site` holds each server's vocabulary frame;
`pooled` is the de-duplicated union across servers.

## Examples

``` r
if (FALSE) { # \dontrun{
vocabs <- ds.omop.vocab.vocabularies()
vocabs$pooled
} # }
```
