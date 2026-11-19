# Inspect the active disclosure thresholds on each server

Reports the disclosure-control thresholds currently in effect on every
connected server, so an analyst or data controller can see the effective
floor each server enforces — most importantly `nfilter_subset`, the
minimum number of distinct persons the per-patient gate
(`.assertMinPersons`) requires before any result is returned.

This is strictly **read-only**. The thresholds are configured
server-side through R options (Opal admin panel, Armadillo config, or
`Rprofile.site`); there is deliberately no client-side way to lower
them. Servers may report different floors: the per-server settings are
in `$per_site`, and the pooled view is the federation contract the
client negotiates from them (the settings every server can honour
together, as used by federated analyses).

## Usage

``` r
ds.omop.disclosure.settings(
  symbol = "omop",
  conns = NULL,
  type = "both",
  datasources = NULL
)
```

## Arguments

  - symbol:
    
    Character; the session symbol (default: `"omop"`).

  - conns:
    
    Deprecated alias of `datasources`, kept for backward compatibility
    (a one-time message names the new argument).

  - type:
    
    Result view: `"both"` (default), `"split"`, or `"combine"`. See
    [dsomop\_conventions](https://isglobal-brge.github.io/dsOMOPClient/reference/dsomop_conventions.md).

  - datasources:
    
    DataSHIELD connections (a named list of `DSConnection` objects).
    `NULL` (the default) uses the OMOP session's connections, or
    `DSI::datashield.connections_find()` when no session exists. See
    [dsomop\_conventions](https://isglobal-brge.github.io/dsOMOPClient/reference/dsomop_conventions.md).

## Value

A `dsomop_result`. `$per_site` is the named list (one element per
server, each holding that server's active disclosure settings, e.g.
`nfilter_subset`, `nfilter_tab`, `nfilter_levels_max`) returned by
earlier versions; servers that fail to respond are omitted and their
errors attached as a `ds_errors` attribute. `res$<server>` still reaches
one server's settings. `$pooled` is the negotiated federation contract
(`NULL`, with `meta$pooling_reason`, when a server did not answer).

## See also

`ds.omop.status`

## Examples

``` r
if (FALSE) { # \dontrun{
settings <- ds.omop.disclosure.settings()
# Effective per-patient floor on each server:
lapply(settings$per_site, function(s) s$nfilter_subset)
# The contract the federation negotiates from them:
settings$pooled
} # }
```
