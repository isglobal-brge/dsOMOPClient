# Filter the rows of a server-side omop.table data frame

Keeps the rows of a server-side, token-keyed data frame that satisfy an
equality/membership condition (`var`, `op`, `value`) or, from 2.10.3, a
language-2 predicate (`expr = omop_where(...)`): the closed grammar `==
!= < <= > >= %in% is.na() ! & |` over the frame's columns, three-valued
(a row whose condition is unknown is dropped), sent as data and
validated and type-checked by the server before any row is read (servers
advertise it as `verb.filter_expr`, dsOMOP 2.10.3). The predicate form
checks that every participating server publishes the dsOMOP 2.10.3
publication protocol and `omopFilterDS`, and advertises
`verb.filter_expr`, before any assignment. An old server or an
incomplete published profile refuses the whole call. Filtering on a
protected/identifier column (the person token or any `dsomop_protected`
column) is rejected server-side. The filtered result is a released
object: its distinct persons must reach the server's `nfilter.subset`;
the excluded rows stay private.

## Usage

``` r
ds.omop.filter(
  x = NULL,
  var = NULL,
  op = NULL,
  value = NULL,
  newobj = NULL,
  symbol = "omop",
  conns = NULL,
  expr = NULL
)
```

## Arguments

  - x:

    Character; name of the server-side `omop.table` symbol. If `NULL`
    (the default), the session's most recently produced output symbol is
    used.

  - var:

    Character; the (non-protected) column to filter on.

  - op:

    Character; one of `"=="`, `"!="`, `"in"`, or `"not_in"` (other
    comparisons: `expr`).

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

  - expr:

    (2.10.3) A predicate from `omop_where` (instead of
    `var`/`op`/`value`).

## Value

Invisibly, the name of the created server-side symbol (`newobj`).

## See also

`ds.omop.select`, `ds.omop.merge`, `ds.omop.mutate`

## Examples

``` r
if (FALSE) { # \dontrun{
women <- ds.omop.filter("features", var = "sex", op = "==", value = "F")
adults <- ds.omop.filter("features", expr = omop_where(age >= 18, !is.na(bmi)))
} # }
```
