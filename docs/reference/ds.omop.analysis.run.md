# Run a unified analysis catalog entry

Executes one catalog entry across every connected server through the
server's single fail-closed run path, which validates and sanitizes
parameters, applies optional population scoping, runs the entry's
compute step (SQL template or wrapped Achilles/OHDSI accessor), and
funnels the result through the ONE per-patient disclosure gate.
Aggregate entries return disclosure-controlled data frames pooled
according to the server-owned contract. Protected components are summed
on their signed basis before display projection; protected effects use
inverse-variance meta-analysis of per-site estimates. Standard count
contracts sum counts with suppression propagation. Assign-mode
QueryLibrary loaders (detected from the entry metadata) instead store
their result server-side and return per-server assignment confirmations.

## Usage

``` r
ds.omop.analysis.run(
  name,
  params = list(),
  cohort = NULL,
  tables = NULL,
  combine = "union",
  pooling_policy = "strict",
  plot = FALSE,
  date_handling = NULL,
  symbol = "omop",
  conns = NULL,
  type = "both"
)
```

## Arguments

  - name:

    Character; the entry id, or a shorthand for it (native id without
    the `"dsomop:"` prefix, or a unique id suffix).

  - params:

    Named list; parameter values for the entry (see
    `ds.omop.analysis.get` for the entry's parameter specs).

  - cohort:

    Optional cohort reference to scope the population to: a
    `dsomop_cohort_handle` (from `ds.omop.cohort.create`,
    `ds.omop.cohort.combine`, or `ds.omop.cohort.from_table`), a
    `cohort_definition_id`, or a server-side cohort table name. `NULL`
    (the default) means no cohort scoping.

  - tables:

    Optional character vector of server-side `omop.table` symbol names
    to scope the population to (their distinct persons). May be combined
    with `cohort`. Each table crosses DataSHIELD as its own bare named
    `scope_table_<n>` argument, never through a generic `list()` or
    `c()` AggregateMethod.

  - combine:

    Character; how to fold multiple scope sources together: `"union"`
    (the default) or `"intersect"`.

  - pooling\_policy:

    Character; how suppressed (NA) cells are handled when pooling
    aggregate results across servers. `"strict"` (default) requires each
    pooled group on every server; `"pooled_only_ok"` pools only
    available disclosure-safe contributions. Kaplan-Meier curves still
    require every preceding risk-set bin because their values are
    cumulative.

  - plot:

    Logical; when `TRUE` AND the entry ships a plot recipe, build a
    `ggplot` CLIENT-SIDE over the pooled, gate-passed data and attach it
    to the result (also returned via the `"plot"` attribute and
    `meta$plot`). `FALSE` (the default) returns data only and never
    touches ggplot2. The plot is purely a client-side rendering of
    numbers that already cleared the server's disclosure gate (see
    Safety, below). A broken/incompatible recipe degrades to a warning
    and a `NULL` plot — it never costs you the returned data.

  - date\_handling:

    For assign-mode loaders, the server-side date policy: `"remove"`
    (default), `"relative"`, `"binned"`, or server-authorized
    `"absolute"`. Ignored for aggregate entries.

  - symbol:

    Character; the session symbol used when the OMOP connection was
    initialised (default: `"omop"`).

  - conns:

    DSI connection object(s). If `NULL` (the default), the connections
    stored in the active session are used.

  - type:

    Result view, following dsBaseClient terminology: `"both"` (default)
    returns per-server and correctly combined results, `"split"` returns
    only per-server results, and `"combine"` returns only the combined
    result. The aliases `"b"`, `"splits"`/`"s"`, and `"combined"`/`"c"`
    are accepted. Combining is performed only from the server-owned
    pooling contract; no column-name heuristics are used.

## Value

A `dsomop_result` object. For aggregate entries, `per_site` holds each
server's disclosure-controlled data frame and `pooled` holds the
cross-server aggregation. For assign-mode entries, `per_site` holds
per-server assignment confirmations (the data stays on the server) and
the server-side symbol name is recorded in the result metadata. When
`plot = TRUE` and the entry ships a plot recipe, the built `ggplot` is
attached as the `"plot"` attribute (and `meta$plot`). `meta$protection`
holds each server's protection label and estimand. On dsOMOP 2.10.3,
`"fixed_synopsis"` declares a bounded-vector epsilon and delta zero
before exact support gates; `"realised_sticky"` is heuristic and
declares neither. A `"mixed"` frame identifies its standard and
protected blocks without a frame-level epsilon or delta. No route claims
an API-transcript guarantee. `"standard"`, `"standard_fallback"` and
`"unavailable"` carry no protected guarantee. Older servers may return
the legacy `"dp"` label under their own contract, with an upgrade
warning. The server's `dsomop.tools.mode` (default `"auto"`) decides it
per question.

## Details

Scoping: pass a `cohort` reference and/or one or more workspace
`omop.table` symbol names in `tables`. Multiple sources are folded
server-side with `combine` (`"union"`/`"intersect"` on the person key)
into a single re-gated cohort, and SQL entries are restricted to it.
Pre-computed Achilles/OHDSI entries hold no per-row person key and
reject scoping (the server raises a clear error).

`name` accepts the full pack-prefixed id (`"dsomop:fe.prevalence"`) and
the natural shorthands: the native id without the prefix
(`"fe.prevalence"`) or a unique id suffix (`"prevalence"`). An ambiguous
shorthand errors with the candidate ids.

## Which tool when

Three layers, simplest first — reach for the lowest one that does the
job:

  - **One-liners** (`ds.omop.prevalence`, `ds.omop.distribution`): the
    fastest path for the two most common summaries over a cohort. One
    call plus good defaults; thin wrappers over this function.

  - **Analysis catalog** (`ds.omop.analysis.list` /
    `ds.omop.analysis.get` / `ds.omop.analysis.run`): the full menu of
    curated, pre-gated analyses (QueryLibrary, Achilles, OHDSI, native
    diagnostics). Use it to discover and run any named analysis with
    explicit params.

  - **Recipes** (`omop_recipe` + `recipe_execute`): author a bespoke
    EXTRACTION — choose populations, variables, filters, and output
    shape — when no single catalog analysis fits. The complete,
    declarative query surface.

## Safety (client-side plotting)

The server may advertise only an allowlisted plot type and declarative
column mappings. The client dispatches to installed local renderers over
already-gated pooled data; it never parses or evaluates source code
received from a server.

## See also

`ds.omop.analysis.list`, `ds.omop.analysis.get`

## Examples

``` r
if (FALSE) { # \dontrun{
# Discover, inspect, then run an entry scoped to a cohort.
catalog <- ds.omop.analysis.list(domain = "condition")
entry   <- catalog$pooled$name[1]
meta    <- ds.omop.analysis.get(entry)
meta$pooled$params

diabetes <- ds.omop.cohort.create(spec = ..., cohort_id = 1)
res <- ds.omop.analysis.run(
  entry,
  params = list(top_n = 25),
  cohort = diabetes
)
res$pooled

# Scope by one or more workspace omop.table symbols, intersected.
res2 <- ds.omop.analysis.run(
  entry,
  tables  = c("my_cohort", "my_other_cohort"),
  combine = "intersect"
)

# Build the entry's client-side plot over the pooled, gate-passed data.
res3 <- ds.omop.analysis.run(entry, params = list(top_n = 25), plot = TRUE)
attr(res3, "plot")   # the ggplot (NULL if the entry ships no plot recipe)
} # }
```
