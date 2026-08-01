# Run a unified analysis catalog entry

Executes one catalog entry across every connected server through the
server's single fail-closed run path, which validates and sanitizes
parameters, applies optional population scoping, runs the entry's
compute step (SQL template or wrapped Achilles/OHDSI accessor), and
funnels the result through the ONE per-patient disclosure gate.
Aggregate entries return disclosure-controlled data frames that are
additionally pooled across servers (count columns are summed with
suppression propagation). Assign-mode QueryLibrary loaders (detected
from the entry metadata) instead store their result server-side and
return per-server assignment confirmations.

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
  conns = NULL
)
```

## Arguments

- name:

  Character; the entry id, or a shorthand for it (native id without the
  `"dsomop:"` prefix, or a unique id suffix).

- params:

  Named list; parameter values for the entry (see
  [`ds.omop.analysis.get`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.analysis.get.md)
  for the entry's parameter specs).

- cohort:

  Optional cohort reference to scope the population to: a
  `dsomop_cohort_handle` (from
  [`ds.omop.cohort.create`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.cohort.create.md),
  [`ds.omop.cohort.combine`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.cohort.combine.md),
  or
  [`ds.omop.cohort.from_table`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.cohort.from_table.md)),
  a `cohort_definition_id`, or a server-side cohort table name. `NULL`
  (the default) means no cohort scoping.

- tables:

  Optional character vector of server-side `omop.table` symbol names to
  scope the population to (their distinct persons). May be combined with
  `cohort`. Each table crosses DataSHIELD as its own bare named
  `scope_table_<n>` argument, never through a generic
  [`list()`](https://rdrr.io/r/base/list.html) or
  [`c()`](https://rdrr.io/r/base/c.html) AggregateMethod.

- combine:

  Character; how to fold multiple scope sources together: `"union"` (the
  default) or `"intersect"`.

- pooling_policy:

  Character; how suppressed (NA) cells are handled when pooling
  aggregate results across servers. `"strict"` (the default) sets the
  pooled value to NA if any server suppressed it; `"pooled_only_ok"`
  sums only the non-suppressed values.

- plot:

  Logical; when `TRUE` AND the entry ships a plot recipe, build a
  `ggplot` CLIENT-SIDE over the pooled, gate-passed data and attach it
  to the result (also returned via the `"plot"` attribute and
  `meta$plot`). `FALSE` (the default) returns data only and never
  touches ggplot2. The plot is purely a client-side rendering of numbers
  that already cleared the server's disclosure gate (see Safety, below).
  A broken/incompatible recipe degrades to a warning and a `NULL` plot —
  it never costs you the returned data.

- date_handling:

  For assign-mode loaders, the server-side date policy: `"remove"`
  (default), `"relative"`, `"binned"`, or server-authorized
  `"absolute"`. Ignored for aggregate entries.

- symbol:

  Character; the session symbol used when the OMOP connection was
  initialised (default: `"omop"`).

- conns:

  DSI connection object(s). If `NULL` (the default), the connections
  stored in the active session are used.

## Value

A `dsomop_result` object. For aggregate entries, `per_site` holds each
server's disclosure-controlled data frame and `pooled` holds the
cross-server aggregation. For assign-mode entries, `per_site` holds
per-server assignment confirmations (the data stays on the server) and
the server-side symbol name is recorded in the result metadata. When
`plot = TRUE` and the entry ships a plot recipe, the built `ggplot` is
attached as the `"plot"` attribute (and `meta$plot`).

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

- **One-liners**
  ([`ds.omop.prevalence`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.prevalence.md),
  [`ds.omop.distribution`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.distribution.md)):
  the fastest path for the two most common summaries over a cohort. One
  call plus good defaults; thin wrappers over this function.

- **Analysis catalog**
  ([`ds.omop.analysis.list`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.analysis.list.md)
  /
  [`ds.omop.analysis.get`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.analysis.get.md)
  / `ds.omop.analysis.run`): the full menu of curated, pre-gated
  analyses (QueryLibrary, Achilles, OHDSI, native diagnostics). Use it
  to discover and run any named analysis with explicit params.

- **Recipes**
  ([`omop_recipe`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_recipe.md) +
  [`recipe_execute`](https://isglobal-brge.github.io/dsOMOPClient/reference/recipe_execute.md)):
  author a bespoke EXTRACTION — choose populations, variables, filters,
  and output shape — when no single catalog analysis fits. The complete,
  declarative query surface.

## Safety (client-side plotting)

The server may advertise only an allowlisted plot type and declarative
column mappings. The client dispatches to installed local renderers over
already-gated pooled data; it never parses or evaluates source code
received from a server.

## See also

[`ds.omop.analysis.list`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.analysis.list.md),
[`ds.omop.analysis.get`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.analysis.get.md)

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
