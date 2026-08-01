# Validate an extraction plan

Sends the plan to each connected server for structural validation,
checking for missing required fields, invalid table references,
unsupported output types, and schema compatibility issues. This performs
a server-side check (via `omopPlanPreviewDS`) but does not execute the
plan or create any data. Use this to catch errors before calling
[`ds.omop.plan.execute`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.execute.md).
Multi-server validation first establishes or revalidates the same strict
schema/semantic harmonization contract used by execution.

## Usage

``` r
ds.omop.plan.validate(plan, symbol = "omop", conns = NULL)
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

A named list (one element per server). Each server's result is the
preview payload, whose `$validation` sub-list reports `valid` (logical),
`errors`, `warnings`, and `available_tables`. No raw rows or SQL are
returned; the shared preview payload can include only the optional
disclosure-banded person count described in
[`ds.omop.plan.preview`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.preview.md).

## Details

Note: `ds.omop.plan.validate` and
[`ds.omop.plan.preview`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.preview.md)
call the same server endpoint (`omopPlanPreviewDS`) and therefore return
the *same* structure; the difference is only intent. Read the
`$validation` element (`valid`/`errors`/`warnings`) for a pass/fail
check here, and the `$outputs` element for the per-output detail under
[`ds.omop.plan.preview`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.preview.md).

## See also

[`ds.omop.plan.preview`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.preview.md),
[`ds.omop.plan.execute`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.execute.md)

## Examples

``` r
if (FALSE) { # \dontrun{
result <- ds.omop.plan.validate(my_plan)
# Check a specific server's pass/fail and messages
result$server1$validation$valid
result$server1$validation$errors
result$server1$validation$warnings
} # }
```
