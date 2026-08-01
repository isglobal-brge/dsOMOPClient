# Get the CDM source description from each server

Returns the full `cdm_source` table row(s) from each connected server,
describing the data source (name, abbreviation, holder, release/version
dates, etc.). This is metadata, not patient data, so this reader is not
disclosure-gated. Sites may describe genuinely different data sources,
so the result is deliberately kept **per-server** and is not merged.

## Usage

``` r
ds.omop.cdm.source(symbol = "omop", conns = NULL, execute = TRUE)
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

A `dsomop_result` with `scope = "per_site"`. Each server's result is its
`cdm_source` data frame (empty if the table is absent). `pooled` is
`NULL` by design.

## Examples

``` r
if (FALSE) { # \dontrun{
src <- ds.omop.cdm.source()
src$per_site            # one cdm_source frame per server
} # }
```
