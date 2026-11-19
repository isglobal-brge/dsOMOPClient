# List available cohort definitions

Queries each connected server for existing cohort definitions. Returns a
named list (one entry per server) of data frames describing the cohorts
that are usable for analysis. This is useful for discovering cohorts
that have already been created in persistent storage.

## Usage

``` r
ds.omop.cohort.list(
  symbol = "omop",
  conns = NULL,
  type = "split",
  datasources = NULL
)
```

## Arguments

  - symbol:
    
    Character; the session symbol used when the OMOP connection was
    initialised (default: `"omop"`).

  - conns:
    
    Deprecated alias of `datasources`, kept for backward compatibility
    (a one-time message names the new argument).

  - type:
    
    Result view: `"split"` (default) or `"both"`. Cohort catalogs are
    site-specific: `"both"` returns `pooled = NULL` with
    `meta$pooling_reason`, and `"combine"` stops before contacting the
    servers.

  - datasources:
    
    DataSHIELD connections (a named list of `DSConnection` objects).
    `NULL` (the default) uses the OMOP session's connections, or
    `DSI::datashield.connections_find()` when no session exists. See
    [dsomop\_conventions](https://isglobal-brge.github.io/dsOMOPClient/reference/dsomop_conventions.md).

## Value

A `dsomop_result`. `$per_site` is the named list (one entry per server)
of data frames with cohort metadata returned by earlier versions, and
`res$<server>` or `res[["<server>"]]` still reaches one server's frame.
Each row is a cohort at or above the server's `nfilter_subset`
threshold; its reported size is BANDED to a multiple of `nfilter_band`
(never the exact subject count). Sub-threshold cohorts are intentionally
absent; a server with no cohort above the threshold returns an empty
data frame.

## Disclosure control

Listing is gated server-side and is intentionally a partial, approximate
view so that discovery is itself disclosure-safe:

  - Only cohorts whose distinct-subject count reaches the server's
    per-subset threshold (`nfilter_subset`) appear. A cohort below that
    threshold is OMITTED entirely – it never shows up in the listing,
    exactly as if it did not exist. You therefore cannot use the listing
    to learn that a small cohort exists.

  - Each surviving cohort's size is BANDED (rounded down to a multiple
    of `nfilter_band`), never the exact subject count, so the listing
    cannot be differenced to recover an individual's membership.

The net effect: you can discover the cohorts you can actually use and
their approximate size, but never tiny cohorts and never an exact count.
The gating and banding happen on the server through the shared
disclosure helpers; the client surfaces the server's response unchanged.

## Examples

``` r
if (FALSE) { # \dontrun{
cohorts <- ds.omop.cohort.list()
cohorts[["server_a"]]
} # }
```
