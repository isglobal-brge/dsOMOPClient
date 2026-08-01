# Disconnect an OMOP session

Closes each server-side database handle, removes its temporary/staged
artifacts and then removes the public OMOP symbol. Every participating
node must report success and symbol removal is verified. On failure the
local session registry is retained so cleanup can be retried.

## Usage

``` r
ds.omop.disconnect(symbol = "omop", conns = NULL)
```

## Arguments

- symbol:

  Character; the session symbol to disconnect (default: "omop").

- conns:

  DSI connections. `NULL` uses the session's stored connections. When
  supplied, it must contain exactly the same named connection objects;
  subsets and replacements are rejected before cleanup.

## Value

Invisible TRUE on success.

## See also

[`ds.omop.connect`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.connect.md)

## Examples

``` r
if (FALSE) { # \dontrun{
ds.omop.disconnect("omop")
} # }
```
