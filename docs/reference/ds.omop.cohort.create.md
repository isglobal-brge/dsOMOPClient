# Create a cohort from a structured specification

Creates a cohort definition on each connected server based on the
provided cohort specification. The cohort is stored as a temporary or
persistent table server-side and can be used to filter subsequent
queries and plan executions. The specification must include a `type`
field and a `concept_set` defining the clinical events that constitute
cohort entry.

## Usage

``` r
ds.omop.cohort.create(
  spec,
  mode = "temporary",
  cohort_id = NULL,
  name = NULL,
  overwrite = FALSE,
  symbol = "omop",
  conns = NULL
)
```

## Arguments

  - spec:

    Named list defining the cohort. Must contain at least `type`
    (character; one of `"condition"`, `"drug"`, `"measurement"`,
    `"observation"`, `"procedure"`) and `concept_set` (integer vector or
    `omop_concept_set` object), and optionally an authenticated
    `value_bin` filter returned by `ds.omop.safe.filter.value()`.

  - mode:

    Character; `"temporary"` (the default) creates a session-scoped temp
    table, `"persistent"` writes to the cohort schema for reuse across
    sessions. Persistent creation is restricted to one server because
    DataSHIELD cannot provide a distributed database commit.

  - cohort\_id:

    Integer; cohort definition ID. If `NULL`, an auto-generated ID is
    used.

  - name:

    Character; human-readable cohort name for display purposes.
    Optional.

  - overwrite:

    Logical; if `TRUE`, an existing cohort with the same `cohort_id`
    will be replaced. Default: `FALSE`.

  - symbol:

    Character; the session symbol used when the OMOP connection was
    initialised (default: `"omop"`).

  - conns:

    DSI connection object(s). If `NULL` (the default), the connections
    stored in the active session are used.

## Value

Invisibly; a `dsomop_cohort_handle` object carrying the deterministic
server-side cohort TABLE name (e.g. `"dsomop_cohort_1"`, or
`"dsomop_cohort_1_ic2"` after two inclusion criteria) for a temporary
cohort, or `NULL` for a persistent cohort. The table is assigned
server-side via `DSI::datashield.assign.expr`. Pass the returned handle
straight into `ds.omop.cohort.combine()`.

## Disclosure control

The resulting cohort is gated server-side on its distinct-subject count:
if the spec (including any `inclusion_criteria`) selects fewer than the
server's per-subset threshold (`nfilter_subset`) persons, creation FAILS
CLOSED with an "insufficient individuals" error and no table is
materialised. Because you authored the criteria, an explicit error here
is expected and carries no disclosure (it only reflects your own spec);
contrast this with the uniform, silent omission used for pre-existing
small cohorts in `ds.omop.cohort.list` / `ds.omop.cohort.definition`.

## Examples

``` r
if (FALSE) { # \dontrun{
diabetes <- ds.omop.cohort.create(
  spec = list(type = "condition",
              concept_set = c(201820, 201826)),
  cohort_id = 1,
  name = "Type 2 Diabetes"
)
# The returned handle feeds directly into ds.omop.cohort.combine():
# ds.omop.cohort.combine(op = "union", cohort_a = diabetes, cohort_b = ...)
} # }
```
