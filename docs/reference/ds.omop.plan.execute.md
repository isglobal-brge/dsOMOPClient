# Execute a plan and create server-side tables

Sends the plan to each connected server for full execution. The
server-side `omopPlanExecuteDS` function processes the plan and assigns
each output directly into the DataSHIELD session as named symbols
specified in the `out` mapping. After execution, the symbols can be used
with standard DataSHIELD analysis functions. Sparse outputs are split
into multiple symbols: `<name>.covariates`, `<name>.covariateRef`, and
`<name>.personRef`. Temporal covariates analogously use
`<name>.temporalCovariates`, `<name>.covariateRef`, `<name>.timeRef`,
and `<name>.personRef`. Person-period outputs additionally assign
`<name>.personPeriods`, the complete episode-by-bin roster.
Recurrent-event survival outputs assign `<name>.events` and
`<name>.riskSets`.

## Usage

``` r
ds.omop.plan.execute(
  plan,
  out = NULL,
  symbol = "omop",
  conns = NULL,
  output_mode = "memory"
)
```

## Arguments

- plan:

  An `omop_plan` object.

- out:

  Optional output-to-symbol mapping. Three forms are accepted:

  - **Missing or `NULL` (default):** symbol names are auto-derived for
    *every* plan output exactly as
    [`recipe_execute`](https://isglobal-brge.github.io/dsOMOPClient/reference/recipe_execute.md)
    does — the output's own `result_symbol` when set, otherwise
    `D_<name>` (so an output named `baseline` becomes symbol
    `D_baseline`). Single-output plans therefore just work with no
    `out`.

  - **A bare unnamed string** (e.g. `out = "D"`): allowed only when the
    plan has exactly *one* output, which is bound to that symbol. With
    multiple outputs this stops with an error asking you to use the
    named form.

  - **A named character vector** (e.g.
    `c(baseline = "D_base", survival = "D_tte")`): maps each named plan
    output to its server-side symbol. This advanced multi-output form is
    unchanged and fully backward compatible.

- symbol:

  Character; name of the OMOP session symbol on the server (default
  `"omop"`).

- conns:

  DSI connection object(s). If `NULL`, uses the connections stored in
  the session.

- output_mode:

  Character; `"memory"` (default, backwards compatible) or `"staged"`
  (writes server-local files and returns descriptors). Arrow provides
  Parquet; without it the server uses CSV.

## Value

Invisible; the resolved `out` symbol mapping (for chaining). The
produced symbols are also recorded on the session so subsequent
manipulation wrappers
([`ds.omop.merge`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.merge.md),
[`ds.omop.filter`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.filter.md),
[`ds.omop.select`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.select.md),
[`ds.omop.bind_rows`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.bind_rows.md))
can default to the last one.

## Details

When `output_mode = "staged"`, outputs are written to server-local
Parquet files (CSV fallback when Arrow is unavailable) and assigned as
`FlowerDatasetDescriptor` objects instead of final data.frames. Long
event and interval outputs preserve numeric OMOP concept IDs and stream
in bounded chunks to Parquet row groups in one file; labels can be
supplied as a separate concept-reference output. Outputs that still
require an R-side reshape or derivation are materialized before staging.
Descriptors are server paths readable under the server OS identity;
other service accounts require a separately reviewed broker. They are
not client download URLs and do not by themselves establish
compatibility with a particular external package. Cleanup is all-or-none
for DataSHIELD-visible symbols, not a distributed filesystem
transaction: after a cross-node staged failure, private files may remain
registered with a successful server handle until its cleanup or
disconnect path runs.

With two or more servers, execution first establishes (or revalidates) a
strict schema/semantic harmonization contract. This binds the plan to
the exact participating nodes, required OMOP columns and compatible type
families; vocabulary-dependent plans additionally require one reported
vocabulary version. All expected output components must land on every
node. A node failure, incomplete composite output, or
factor-harmonization failure removes the exact symbols owned by the
attempted execution and fails closed. Every requested output family must
be absent on every server before execution; existing workspace objects
are never deleted or overwritten speculatively. Choose fresh `out` names
(or explicitly remove obsolete objects through the ordinary DataSHIELD
workspace API) when rerunning a plan.

## See also

[`ds.omop.plan.validate`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.validate.md),
[`ds.omop.plan.preview`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.preview.md)

## Examples

``` r
if (FALSE) { # \dontrun{
plan <- ds.omop.plan()
plan <- ds.omop.plan.baseline(plan)

# Simplest case: single-output plan, bind to "D".
ds.omop.plan.execute(plan, out = "D")

# Or omit out entirely to auto-derive D_<name> for every output.
ds.omop.plan.execute(plan)

# Advanced: multiple outputs, each mapped explicitly.
plan <- ds.omop.plan.events(plan, "conditions",
  "condition_occurrence", concept_set = c(201826))
ds.omop.plan.execute(plan,
  out = c(baseline = "D_base", conditions = "D_cond")
)

# Staged mode for large extractions
ds.omop.plan.execute(plan,
  out = c(features = "D_features"),
  output_mode = "staged"
)
# D_features is now a FlowerDatasetDescriptor on the server
} # }
```
