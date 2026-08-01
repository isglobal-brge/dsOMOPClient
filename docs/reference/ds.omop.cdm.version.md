# Get the CDM version reported by each server

Returns the CDM version reported by each connected server (preferring
`cdm_source.cdm_version` and falling back to the version inferred from
the table structure). This is metadata, not patient data, so this reader
is not disclosure-gated. Sites may legitimately run different CDM
versions, so the result is deliberately kept **per-server** and is never
silently merged into a single version.

## Usage

``` r
ds.omop.cdm.version(symbol = "omop", conns = NULL, execute = TRUE)
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

A `dsomop_result` with `scope = "per_site"`. Each server's result is a
list with `cdm_version`, `source`, and `vocabulary_version`. `pooled` is
`NULL` by design.

## Examples

``` r
if (FALSE) { # \dontrun{
ver <- ds.omop.cdm.version()
# CDM version per server
lapply(ver$per_site, function(v) v$cdm_version)
} # }
```
