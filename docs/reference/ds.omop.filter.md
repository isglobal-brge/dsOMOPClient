# Filter the rows of a server-side omop.table data frame

Applies a categorical equality/membership filter to a server-side,
token-keyed data frame. Filtering on a protected/identifier column (the
person token or any `dsomop_protected` column) is rejected server-side.
The server re-gates the filtered result on its distinct-person count and
fails closed if the filter narrows the population below the disclosure
threshold.

## Usage

``` r
ds.omop.filter(
  x = NULL,
  var,
  op,
  value,
  newobj = NULL,
  symbol = "omop",
  conns = NULL
)
```

## Arguments

- x:

  Character; name of the server-side `omop.table` symbol. If `NULL` (the
  default), the session's most recently produced output symbol is used.

- var:

  Character; the (non-protected) column to filter on.

- op:

  Character; one of `"=="`, `"!="`, `"in"`, or `"not_in"`. Numeric/date
  thresholds belong in recipe filters with disclosure-safe bins/ranges.

- value:

  Category value(s).

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

[`ds.omop.select`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.select.md),
[`ds.omop.merge`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.merge.md)

## Examples

``` r
if (FALSE) { # \dontrun{
women <- ds.omop.filter("features", var = "sex", op = "==", value = "F")
} # }
```
