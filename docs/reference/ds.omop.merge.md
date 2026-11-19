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
  conns = NULL,
  join = NULL,
  datasources = NULL
)
```

## Arguments

  - x:
    
    Character; name of the left server-side `omop.table` symbol (e.g.
    the `newobj` returned by a previous verb, or a symbol created by
    `ds.omop.plan.execute`). If `NULL` (the default), the session's most
    recently produced output symbol is used.

  - y:
    
    Character; name of the right server-side `omop.table` symbol.

  - by:
    
    Character vector; the join key. Must be the person key
    (`"person_id"` or `"subject_id"`); other columns are rejected
    server-side.

  - type:
    
    Deprecated alias of `join` (kept in its position, so positional
    calls keep working).

  - newobj:
    
    Character; name of the server-side symbol to create. If `NULL`
    (default), a unique name is generated.

  - symbol:
    
    Character; the session symbol used when the OMOP connection was
    initialised (default: `"omop"`).

  - conns:
    
    Deprecated alias of `datasources`, kept for backward compatibility
    (a one-time message names the new argument).

  - join:
    
    Character; the join type, `"inner"` (default) or `"left"`. It
    replaces `type`, which elsewhere in the package names the result
    view.

  - datasources:
    
    DataSHIELD connections (a named list of `DSConnection` objects).
    `NULL` (the default) uses the OMOP session's connections, or
    `DSI::datashield.connections_find()` when no session exists. See
    [dsomop\_conventions](https://isglobal-brge.github.io/dsOMOPClient/reference/dsomop_conventions.md).

## Value

Invisibly, the name of the created server-side symbol (`newobj`).

## See also

`ds.omop.filter`, `ds.omop.bind_rows`

## Examples

``` r
if (FALSE) { # \dontrun{
joined <- ds.omop.merge("cohort_a", "cohort_b", join = "left")
} # }
```
