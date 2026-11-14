# Check OHDSI result tool availability

Queries each connected server to determine which OHDSI tool result
tables (CohortDiagnostics, CohortIncidence, Characterization, and
others) are present.

## Usage

``` r
ds.omop.ohdsi.status(symbol = "omop", conns = NULL, type = NULL)
```

## Arguments

  - symbol:

    Character; the session symbol (default: `"omop"`).

  - conns:

    DSI connection object(s). If `NULL`, uses active session.

  - type:

    Optional result view: `"split"`, `"combine"`, or `"both"`. Status is
    site-specific, so a combined view has no value and records that
    reason.

## Value

A `dsomop_result` object with `scope = "per_site"`.

## Examples

``` r
if (FALSE) { # \dontrun{
status <- ds.omop.ohdsi.status()
status$per_site[["server_a"]]$cohort_diagnostics$available
} # }
```
