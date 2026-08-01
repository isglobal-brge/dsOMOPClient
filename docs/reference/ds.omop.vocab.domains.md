# List the domains available on each server

Returns the distinct domains from the OMOP `domain` table on each
connected server (falling back to distinct `domain_id` values on
`concept` when the domain table is not loaded). Vocabulary reference
data carries no patient information, so this reader is not
disclosure-gated and the per-site frames are pooled by set union.

## Usage

``` r
ds.omop.vocab.domains(symbol = "omop", conns = NULL, execute = TRUE)
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

A `dsomop_result`. `per_site` holds each server's domain frame; `pooled`
is the de-duplicated union across servers.

## Examples

``` r
if (FALSE) { # \dontrun{
domains <- ds.omop.vocab.domains()
domains$pooled
} # }
```
