# Preview a plan (server-side dry run)

Sends the plan to each connected server for a dry-run preview that, per
output, reports its expected/resolvable columns and any
requested-but-missing source columns without creating output data. A
disclosure-banded distinct-person count is included only when it can be
computed honestly from an unscoped, unfiltered, unreduced source.
Cohort-, population-, filter-, temporal- or feature-scoped outputs
instead return `n_persons = NA`, `n_persons_available = FALSE`, and a
reason; they are never labelled with the whole source table's
population. Available counts are banded down to a multiple of the
server's `band_width` and suppressed below the disclosure floor. Raw row
counts, min/max and SQL are never returned. Multi-server preview first
establishes or revalidates the same strict schema/semantic harmonization
contract used by execution.

## Usage

``` r
ds.omop.plan.preview(plan, symbol = "omop", conns = NULL)
```

## Arguments

- plan:

  An `omop_plan` object.

- symbol:

  Character; name of the OMOP session symbol on the server (default
  `"omop"`).

- conns:

  DSI connection object(s). If `NULL`, uses the connections stored in
  the session.

## Value

A named list (one element per server). Each server's result holds
`$validation` (see
[`ds.omop.plan.validate`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.validate.md)),
`$band_width` (the count-banding granularity), and `$outputs`, a
per-output list with `columns`, `missing_columns`, `n_persons` (banded
when available), `n_persons_available`, `n_persons_unavailable_reason`,
`n_persons_banded`, `disclosive`, and `representation`.

## Details

Note: `ds.omop.plan.preview` and
[`ds.omop.plan.validate`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.validate.md)
call the same server endpoint (`omopPlanPreviewDS`) and return the same
structure; the distinction is only intent. Read `$outputs` here for
per-output detail, and `$validation` under
[`ds.omop.plan.validate`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.validate.md)
for the pass/fail check.

## See also

[`ds.omop.plan.validate`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.validate.md),
[`ds.omop.plan.execute`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.execute.md)

## Examples

``` r
if (FALSE) { # \dontrun{
preview <- ds.omop.plan.preview(my_plan)
# Resolvable columns for the "baseline" output on one server
preview$server1$outputs$baseline$columns
# Disclosure-banded count, or NA plus a reason for scoped outputs
preview$server1$outputs$baseline$n_persons
preview$server1$outputs$baseline$n_persons_available
preview$server1$outputs$baseline$n_persons_unavailable_reason
} # }
```
