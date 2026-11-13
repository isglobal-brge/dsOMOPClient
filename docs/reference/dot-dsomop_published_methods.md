# The dsOMOP methods each server's profile publishes

The dsOMOP methods each server's profile publishes

## Usage

``` r
.dsomop_published_methods(conns)
```

## Arguments

  - conns:

    DSI connections.

## Value

Named list per server: `version` (versions recorded with the published
dsOMOP methods), `aggregate`, `assign`, or `error` when the profile
could not be listed.
