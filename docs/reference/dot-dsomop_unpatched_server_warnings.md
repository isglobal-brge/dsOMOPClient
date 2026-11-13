# Plain warnings for servers older than dsOMOP 2.10.3

Such a server lacks the dsOMOP 2.10.3 security fixes of its OHDSI
community tools (analysis catalogue, Achilles and OHDSI readers, DP
releases); a newer client cannot repair a server, and never claims to.

## Usage

``` r
.dsomop_unpatched_server_warnings(versions)
```

## Arguments

  - versions:

    Named character vector of server dsOMOP versions (NA when unknown).

## Value

Character vector of warnings, one per server older than 2.10.3.
