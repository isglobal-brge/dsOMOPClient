# Per-server reasons for a failed DataSHIELD step

The servers' own messages, as reported to DSI's error callback or kept
by `DSI::datashield.errors()`, instead of DSI's generic "There are some
DataSHIELD errors" notice; each followed by what to do when that is
known.

## Usage

``` r
.ds_failure_detail(errors = list(), condition = NULL, servers = character(0))
```

## Arguments

  - errors:

    Named list or character vector of messages per server.

  - condition:

    Optional condition raised by DSI.

  - servers:

    Servers that did not succeed.

## Value

Character; one line per server.
