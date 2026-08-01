# Merge two server-side omop.table data frames on the person key

Performs an inner or left join of two server-side, token-keyed data
frames on the per-session person token. The join is restricted to the
person key so a merge cannot correlate individuals on any other
quasi-identifier. The server re-gates the joined result on its
distinct-person count and fails closed if it falls below the disclosure
threshold.

## Usage

``` r
ds.omop.merge(
  x = NULL,
  y,
  by = "person_id",
  type = "inner",
  newobj = NULL,
  symbol = "omop",
  conns = NULL
)
```

## Arguments

- x:

  Character; name of the left server-side `omop.table` symbol (e.g. the
  `newobj` returned by a previous verb, or a symbol created by
  [`ds.omop.plan.execute`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.execute.md)).
  If `NULL` (the default), the session's most recently produced output
  symbol is used.

- y:

  Character; name of the right server-side `omop.table` symbol.

- by:

  Character vector; the join key. Must be the person key (`"person_id"`
  or `"subject_id"`); other columns are rejected server-side.

- type:

  Character; `"inner"` (default) or `"left"`.

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

[`ds.omop.filter`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.filter.md),
[`ds.omop.bind_rows`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.bind_rows.md)

## Examples

``` r
if (FALSE) { # \dontrun{
joined <- ds.omop.merge("cohort_a", "cohort_b", type = "left")
} # }
```
