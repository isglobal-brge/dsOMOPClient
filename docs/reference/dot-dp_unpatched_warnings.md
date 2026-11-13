# Plain warnings for DP servers older than dsOMOP 2.10.3

A server that predates dsOMOP 2.10.3 lacks its security fixes of the
OHDSI community tools; its DP answers follow its legacy contract. The
client says so plainly and never implies that a newer client repairs a
server.

## Usage

``` r
.dp_unpatched_warnings(statuses)
```

## Arguments

  - statuses:

    Result of `.dp_statuses`.

## Value

Character vector of warning messages (one per unpatched server).
