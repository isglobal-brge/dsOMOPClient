# Check Achilles availability

Queries each connected server to determine whether Achilles result
tables (`achilles_results` and `achilles_results_dist`) are present and
accessible. Returns per-site availability status and table row counts.
No pooling is performed because this is metadata-only.

## Usage

``` r
ds.omop.achilles.status(symbol = "omop", conns = NULL, type = NULL)
```

## Arguments

  - symbol:

    Character; the session symbol used when the OMOP connection was
    initialised (default: `"omop"`).

  - conns:

    DSI connection object(s). If `NULL` (the default), the connections
    stored in the active session are used.

  - type:

    Optional result view: `"split"`, `"combine"`, or `"both"`. Status is
    site-specific, so a combined view has no value and records that
    reason.

## Value

A `dsomop_result` object with `scope = "per_site"`. Each server's result
is a list or data frame indicating whether the Achilles tables exist and
how many rows they contain.

## Examples

``` r
if (FALSE) { # \dontrun{
status <- ds.omop.achilles.status()
status$per_site[["server_a"]]$available
} # }
```
