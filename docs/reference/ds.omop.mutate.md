# Derive columns of a server-side omop.table

Adds or replaces columns of a server-side, token-keyed data frame with R
expressions over its columns, evaluated on every server by the closed
mutation grammar of `omopMutateDS` (design section 2.11): bare names are
columns (of the input or assigned earlier in the same call); literals
are numbers (`5L` is an integer), strings, `TRUE`/`FALSE`, `NA` and
`as.Date("YYYY-MM-DD")`; operators are `+ - * /` (division by zero or a
non-finite result gives `NA`; `date - date` gives days, `date +/-
integer` a date), conditions (`== != < <= > >= %in% is.na() ! & |`,
three-valued: `NA` when unknown, each comparing one column with one
literal), `ifelse(test, yes, no)`, `case_when(cond ~ value, ...,
.default = )` (an unknown condition gives `NA`), `coalesce(...)` and
`cut(x, breaks, labels, right)` (R's `right = TRUE` default; text
labels, `NA` outside the breaks; `breaks`/`labels` may name vectors of
your session). Integer arithmetic uses exact signed 64-bit storage when
needed; results beyond the ordinary R integer range are retained, and
checked 64-bit overflow yields typed NA. Branches and coalescing
preserve integer64 and datetime values. Repeated explicit cut labels
group their intervals. Nothing is evaluated as R code, on the client or
on the server.

## Usage

``` r
ds.omop.mutate(x = NULL, ..., newobj = NULL, symbol = "omop", conns = NULL)
```

## Arguments

  - x:

    Name of the server-side `omop.table` symbol (default: the session's
    most recent output).

  - ...:

    Named expressions, `name = expression`.

  - newobj:

    Name of the symbol to create (default: a fresh name); `newobj = x`
    replaces `x`.

  - symbol:

    Session symbol (default `"omop"`).

  - conns:

    DSI connections (default: the session's).

## Value

Invisibly, the name of the server-side symbol.

## Details

Person, episode and set keys and protected columns can be neither read
nor written. Class-1 source and assignment names are also refused, even
when the administrator permits their direct projection. The result is a
released object: its distinct persons must reach the server's
`nfilter.subset` (the server refuses it otherwise, with its constant
message) and it carries no differential-privacy provenance. With `newobj
= x` the target is replaced in place on every server; a rollback copy is
kept until every server has succeeded, so a failure on one server
restores the original everywhere. If restoration cannot be confirmed,
the error names the unresolved sites and the retained recovery symbol. A
cleanup error after confirmed commit states that the mutation committed
and identifies the remaining backup cleanup. Every participating server
must publish the dsOMOP 2.10.3 publication protocol and `omopMutateDS`,
and advertise `verb.mutate`. The client checks this before any
assignment, including a rollback copy; an old server or an incomplete
published profile refuses the whole call.

## See also

`ds.omop.filter`, `ds.omop.select`

## Examples

``` r
if (FALSE) { # \dontrun{
ds.omop.mutate("D_cov", pdc = covered / observed,
               adherent = ifelse(pdc >= 0.8, 1L, 0L),
               age_band = cut(age, c(0, 18, 65, 120), right = FALSE))
} # }
```
