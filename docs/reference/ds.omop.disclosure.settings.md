# Inspect the active disclosure thresholds on each server

Reports the disclosure-control thresholds currently in effect on every
connected server, so an analyst or data controller can see the effective
floor each server enforces — most importantly `nfilter_subset`, the
minimum number of distinct persons the per-patient gate
(`.assertMinPersons`) requires before any result is returned.

This is strictly **read-only**. The thresholds are configured
server-side through R options (Opal admin panel, Armadillo config, or
`Rprofile.site`); there is deliberately no client-side way to lower
them. Servers may report different floors, so the result is per-server.

## Usage

``` r
ds.omop.disclosure.settings(symbol = "omop", conns = NULL)
```

## Arguments

- symbol:

  Character; the session symbol (default: `"omop"`).

- conns:

  DSI connection object(s) or `NULL` to use the session default.

## Value

A named list, one element per server, each holding that server's active
disclosure settings (e.g. `nfilter_subset`, `nfilter_tab`,
`nfilter_levels_max`). Servers that fail to respond are omitted and
their errors attached as a `ds_errors` attribute.

## See also

[`ds.omop.status`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.status.md)

## Examples

``` r
if (FALSE) { # \dontrun{
settings <- ds.omop.disclosure.settings()
# Effective per-patient floor on each server:
lapply(settings, function(s) s$nfilter_subset)
} # }
```
