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
`<name>.riskSets`. Multi-state outputs assign `<name>.msdata` and the
public transition dictionary `<name>.transitionRef`.

## Usage

``` r
ds.omop.plan.execute(
  plan,
  out = NULL,
  symbol = "omop",
  conns = NULL,
  output_mode = "memory",
  on_small_site = c("error", "skip")
)
```

## Arguments

  - plan:

    An `omop_plan` object, or a language-2 plan (`omop_plan_v2`, from
    `recipe_to_plan`). A v2 plan needs every server to advertise recipe
    language 2 and every operation it uses; it is sent losslessly and
    published through the same status protocol.

  - out:

    Optional output-to-symbol mapping. Three forms are accepted:

      - **Missing or `NULL` (default):** symbol names are auto-derived
        for *every* plan output exactly as `recipe_execute` does — the
        output's own `result_symbol` when set, otherwise `D_<name>` (so
        an output named `baseline` becomes symbol `D_baseline`).
        Single-output plans therefore just work with no `out`.

      - **A bare unnamed string** (e.g. `out = "D"`): allowed only when
        the plan has exactly *one* output, which is bound to that
        symbol. With multiple outputs this stops with an error asking
        you to use the named form.

      - **A named character vector** (e.g. `c(baseline = "D_base",
        survival = "D_tte")`): maps each named plan output to its
        server-side symbol. This advanced multi-output form is unchanged
        and fully backward compatible.

  - symbol:

    Character; name of the OMOP session symbol on the server (default
    `"omop"`).

  - conns:

    DSI connection object(s). If `NULL`, uses the connections stored in
    the session.

  - output\_mode:

    Character; `"memory"` (default, backwards compatible) or `"staged"`
    (writes server-local files and returns descriptors). Arrow provides
    Parquet; without it the server uses CSV.

  - on\_small\_site:

    Character; `"error"` (default) or `"skip"`. Only insufficient final
    person support is skippable. Skip requests an all-or-none server
    commit: a rejected site publishes nothing and returns `NULL`. With
    the default, a non-strict server may publish complete passing output
    families and warn about rejected families. A strict refusal rolls
    back the federation. Other failures always roll back and error.
    These policies require the publication protocol on every site. If
    any site lacks it, a v1 plan uses the 2.10.2 legacy call everywhere
    with one warning, and `on_small_site` does not change that call; a
    language-2 plan is refused before assignment.

## Value

Invisibly, the resolved `out` symbol mapping for a complete ordinary
execution. When sites publish different outputs or fresh names, a named
per-site list of actual publication inventories; skipped sites have
`NULL` entries. The produced symbols are also recorded on the session so
subsequent manipulation wrappers (`ds.omop.merge`, `ds.omop.filter`,
`ds.omop.select`, `ds.omop.bind_rows`) can default to the last one.

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
compatibility with a particular external package. Publication status is
read before the temporary execution symbol is removed. The server
reports the actual output inventory and allocates fresh names for
occupied destinations. A technical or transport failure revokes and
removes this execution's symbols and staged files across all
participating sites.

With two or more servers, execution first establishes (or revalidates) a
strict schema/semantic harmonization contract. Concept factors are
harmonized only over the retained sites and their published components.

Loading tables never depends on differential privacy. After a memory
execution, concept columns are converted to factors harmonized across
servers (see `ds.omop.plan.options`).

## See also

`ds.omop.plan.validate`, `ds.omop.plan.preview`

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
