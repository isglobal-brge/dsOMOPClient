# Row-bind two schema-identical server-side omop.table data frames

Stacks two server-side, token-keyed data frames that share an identical
set of column names. The server re-gates the bound result on its
DISTINCT person count: because the gate counts distinct persons rather
than rows, binding a frame to itself cannot inflate the count and the
result is still blocked when it covers too few individuals.

## Usage

``` r
ds.omop.bind_rows(x = NULL, y, newobj = NULL, symbol = "omop", conns = NULL)
```

## Arguments

- x:

  Character; name of the first (top) server-side `omop.table` symbol. If
  `NULL` (the default), the session's most recently produced output
  symbol is used.

- y:

  Character; name of the second (bottom) server-side `omop.table` symbol
  (must have identical column names to `x` server-side).

- newobj:

  Character; name of the server-side symbol to create. If `NULL`
  (default), a unique name is generated.

- symbol:

  Character; the session symbol used when the OMOP connection was
  initialised (default: `"omop"`).

- conns:

  DSI connection object(s). If `NULL` (the default), the connections
  stored in the active session are used.

## Value

Invisibly, the name of the created server-side symbol (`newobj`).

## See also

[`ds.omop.merge`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.merge.md)

## Examples

``` r
if (FALSE) { # \dontrun{
stacked <- ds.omop.bind_rows("wave1", "wave2")
} # }
```
