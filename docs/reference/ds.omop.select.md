# Keep a subset of columns of a server-side omop.table data frame

Projects a server-side, token-keyed data frame to `cols`. The person
token column is always retained server-side so the result stays a valid,
joinable `omop.table`.

## Usage

``` r
ds.omop.select(x = NULL, cols, newobj = NULL, symbol = "omop", conns = NULL)
```

## Arguments

- x:

  Character; name of the server-side `omop.table` symbol. If `NULL` (the
  default), the session's most recently produced output symbol is used.

- cols:

  Character vector; the columns to keep.

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

[`ds.omop.filter`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.filter.md)

## Examples

``` r
if (FALSE) { # \dontrun{
slim <- ds.omop.select("features", cols = c("age", "sex"))
} # }
```
