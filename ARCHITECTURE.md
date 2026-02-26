# dsOMOPClient v2.0.0 — Complete Architecture Report

## Table of Contents

1. [Overview](#1-overview)
2. [Package Metadata](#2-package-metadata)
3. [Core Infrastructure](#3-core-infrastructure)
   - 3.1 [utils.R — Internal Utilities](#31-utilsr--internal-utilities)
   - 3.2 [result.R — dsomop_result S3 Class](#32-resultr--dsomop_result-s3-class)
   - 3.3 [session.R — Connection Lifecycle](#33-sessionr--connection-lifecycle)
   - 3.4 [pooling.R — Client-Side Pooling Engine](#34-poolingr--client-side-pooling-engine)
4. [Data Access Layer](#4-data-access-layer)
   - 4.1 [dictionary.R — Schema Browsing](#41-dictionaryr--schema-browsing)
   - 4.2 [profiling.R — Safe Data Summary Wrappers](#42-profilingr--safe-data-summary-wrappers)
   - 4.3 [exploration.R — Exploration Wrappers](#43-explorationr--exploration-wrappers)
   - 4.4 [vocabulary.R — Vocabulary Operations](#44-vocabularyr--vocabulary-operations)
   - 4.5 [catalog.R — Catalog Query System](#45-catalogr--catalog-query-system)
   - 4.6 [achilles.R — Achilles Pre-Computed Statistics](#46-achillesr--achilles-pre-computed-statistics)
5. [Plan & Cart System](#5-plan--cart-system)
   - 5.1 [plan.R — Extraction Plan DSL](#51-planr--extraction-plan-dsl)
   - 5.2 [cart.R — Cart / Recipe Infrastructure](#52-cartr--cart--recipe-infrastructure)
   - 5.3 [feature.R — Feature Recipe Helpers](#53-featurer--feature-recipe-helpers)
   - 5.4 [cohort.R — Cohort Management](#54-cohortr--cohort-management)
6. [OMOP Studio (Shiny Application)](#6-omop-studio-shiny-application)
   - 6.1 [studio.R — Main App + Shared Utilities](#61-studior--main-app--shared-utilities)
   - 6.2 [studio_connections.R — Module 1: Connections](#62-studio_connectionsr--module-1-connections)
   - 6.3 [studio_explore.R — Module 2: Explore](#63-studio_explorer--module-2-explore)
   - 6.4 [studio_drilldown.R — Module 3: Drilldown](#64-studio_drilldownr--module-3-drilldown)
   - 6.5 [studio_locator.R — Module 4: Concept Locator](#65-studio_locatorr--module-4-concept-locator)
   - 6.6 [studio_vocab.R — Module 5: Vocabulary Browser](#66-studio_vocabr--module-5-vocabulary-browser)
   - 6.7 [studio_atlas.R — Module 6: Data Sources (Atlas)](#67-studio_atlasr--module-6-data-sources-atlas)
   - 6.8 [studio_catalog.R — Module 7: Catalog](#68-studio_catalogr--module-7-catalog)
   - 6.9 [studio_basket.R — Module 8: Basket (Cart UI)](#69-studio_basketr--module-8-basket-cart-ui)
   - 6.10 [studio_plan.R — Module 9: Plan Builder](#610-studio_planr--module-9-plan-builder)
   - 6.11 [studio_script.R — Module 10: Script](#611-studio_scriptr--module-10-script)
   - 6.12 [studio_session.R — Module 11: Session](#612-studio_sessionr--module-11-session)
   - 6.13 [studio_codegen.R — Code Generation Helpers](#613-studio_codegenr--code-generation-helpers)
7. [Multi-Server Architecture](#7-multi-server-architecture)
8. [Data Flow Diagram](#8-data-flow-diagram)

---

## 1. Overview

**dsOMOPClient** is an R package that provides a DataSHIELD-compliant client for interacting with OMOP CDM (Observational Medical Outcomes Partnership Common Data Model) databases in a federated analysis environment. In DataSHIELD, individual-level data never leaves the remote server — only aggregate, disclosure-safe results are returned to the client.

The package has three main layers:

```
┌────────────────────────────────────────────────────────┐
│  OMOP Studio (Shiny UI)                                │
│  11 modules: Connections, Explore, Drilldown, Locator, │
│  Vocabulary, Atlas, Catalog, Basket, Plan, Script,     │
│  Session                                               │
├────────────────────────────────────────────────────────┤
│  Plan & Cart API                                       │
│  plan.R, cart.R, feature.R, cohort.R                   │
├────────────────────────────────────────────────────────┤
│  Data Access Layer                                     │
│  dictionary.R, profiling.R, exploration.R,             │
│  vocabulary.R, catalog.R, achilles.R                   │
├────────────────────────────────────────────────────────┤
│  Core Infrastructure                                   │
│  utils.R, result.R, session.R, pooling.R               │
├────────────────────────────────────────────────────────┤
│  DSI (DataSHIELD Interface)                            │
│  datashield.aggregate(), datashield.assign.expr()      │
└────────────────────────────────────────────────────────┘
```

All communication with remote servers goes through DSI (`datashield.aggregate` for read-only queries, `datashield.assign.expr` for server-side mutations). The server-side companion package (`dsOMOP`) exposes `*DS()` functions that the client calls remotely.

---

## 2. Package Metadata

**File**: `DESCRIPTION`

```
Package: dsOMOPClient
Version: 2.0.0
Depends: R (>= 4.1.0)
Imports: DSI, methods, stats, shiny (>= 1.7.0), bslib (>= 0.5.0), DT, jsonlite
Suggests: DSLite, testthat (>= 3.0.0), shinytest2, chromote, clipr, knitr, rmarkdown
```

**File**: `NAMESPACE` — Exports 109 symbols including:
- All `ds.omop.*` functions (connect, status, tables, columns, concept.search, etc.)
- All `cart_*` functions (add, remove, to_plan, to_code, export/import JSON, etc.)
- All `omop_*` constructors (omop_variable, omop_filter, omop_cart, etc.)
- All `omop.feature.*` helpers (boolean, count, latest_value, etc.)
- S3 methods for `print`, `$`, `as.data.frame` on dsomop_result, omop_cart, omop_plan, etc.
- `ds.omop.studio` (the Shiny app launcher)

---

## 3. Core Infrastructure

### 3.1 `utils.R` — Internal Utilities

**File**: `R/utils.R` (57 lines)

Contains foundational helpers used throughout the package.

#### `%||%` — Null coalescing operator
```r
`%||%` <- function(x, y) if (is.null(x)) y else x
```
Returns `y` when `x` is NULL. Used pervasively for default values.

#### `.dsomop_client_env` — Private session store
```r
.dsomop_client_env <- new.env(parent = emptyenv())
```
A private environment that stores active OMOP sessions by their symbol name. Prevents pollution of the global environment.

#### `.get_session(symbol)` — Session retrieval
```r
.get_session <- function(symbol = "omop") {
  if (!exists(symbol, envir = .dsomop_client_env))
    stop("No OMOP session '", symbol, "'. Call ds.omop.connect() first.")
  get(symbol, envir = .dsomop_client_env)
}
```
Every API function starts by calling `.get_session()` to retrieve the connection handle, DSI connections object, and server metadata.

#### `.generate_symbol(prefix)` — Unique symbol generator
Generates a random 6-character alphanumeric string prefixed with `prefix` (default `"dsO"`). Used for temporary server-side variable names.

#### `.format_r_value(x)` — Code-safe value formatting
Converts an R value to a string suitable for inclusion in generated R code:
- `NULL` → `"NULL"`
- `"hello"` → `'"hello"'`
- `42` → `"42"`
- `c(1, 2, 3)` → `"c(1, 2, 3)"`

#### `.build_code(fn_name, ...)` — Function call code builder
Constructs a reproducible R function call string from a function name and named arguments. Skips NULL arguments.

```r
.build_code("ds.omop.concept.prevalence", table = "measurement", top_n = 50)
# → 'ds.omop.concept.prevalence(table = "measurement", top_n = 50)'
```

---

### 3.2 `result.R` — dsomop_result S3 Class

**File**: `R/result.R` (127 lines)

The `dsomop_result` is the standard return type for all data-fetching functions. It wraps multi-server results into a structured object.

#### Constructor: `dsomop_result(per_site, pooled, meta)`

```r
dsomop_result <- function(per_site, pooled = NULL, meta = list()) {
  obj <- list(
    per_site = per_site,          # Named list: server_name → raw result
    pooled   = pooled,            # NULL or single aggregated data.frame/list
    meta     = list(
      call_code      = meta$call_code %||% "",      # Reproducible R code
      timestamp      = Sys.time(),
      servers        = names(per_site),
      scope          = meta$scope %||% "per_site",
      pooling_policy = meta$pooling_policy %||% "strict",
      warnings       = meta$warnings %||% character(0)
    )
  )
  class(obj) <- c("dsomop_result", "list")
  obj
}
```

**Design rationale**: Every query runs on all connected servers simultaneously (via DSI). The raw per-server results are always stored in `$per_site`. If `scope = "pooled"` was requested, the pooling engine aggregates them and stores the result in `$pooled`. The `$meta$call_code` field captures the equivalent R code so users can reproduce the operation outside of Studio.

#### S3 Methods

| Method | Behavior |
|---|---|
| `print.dsomop_result(x)` | Shows servers, scope, pooled status, warnings, and code snippet |
| `$.dsomop_result(x, name)` | If `name` is "per_site"/"pooled"/"meta", returns that field; otherwise falls through to `per_site[[name]]` for backward compatibility (e.g., `result$server_a`) |
| `as.data.frame.dsomop_result(x)` | Returns pooled result if available, otherwise first server's data.frame |

#### Utility functions

| Function | Purpose |
|---|---|
| `ds.omop.code(x)` | Extract the reproducible R code string from a result |
| `ds.omop.copy_code(x)` | Copy the code to clipboard (uses `clipr` if available) |

---

### 3.3 `session.R` — Connection Lifecycle

**File**: `R/session.R` (180 lines)

Manages the full lifecycle of an OMOP CDM session: connect, disconnect, and status check.

#### `ds.omop.connect(resource, symbol, ...)`

**Purpose**: Establishes a connection to one or more OMOP CDM databases through DataSHIELD.

**Pseudocode**:
```
1. Get DSI connections (from default or explicit `conns`)
2. Find server names via DSI::datashield.connections_find()
3. Map resources to servers (single string → same resource on all; named list → per-server)
4. For each server:
   a. DSI::datashield.assign.resource(conns, symbol=res_symbol, resource=res_name)
5. If strict=TRUE and any failed → stop()
6. Call omopInitDS() on server (with optional schema overrides)
7. Fetch capabilities via omopGetCapabilitiesDS()
8. Clean up temp resource symbol
9. Build omop_session object: {symbol, res_symbol, resource_map, conns, capabilities, server_names, errors}
10. Store in .dsomop_client_env
```

**Key detail**: The `capabilities` object contains per-server metadata (available tables, CDM version, DBMS type, cdm_info) and is cached for the session lifetime. This avoids repeated metadata queries.

#### `ds.omop.disconnect(symbol)`

Calls `omopCleanupDS()` to release server-side resources, removes the symbol from the DataSHIELD session, and removes the entry from `.dsomop_client_env`.

#### `ds.omop.status(symbol)`

Pings all servers via `omopPingDS()` and returns a status summary:
```r
list(symbol, servers, capabilities, ping, errors)
```

---

### 3.4 `pooling.R` — Client-Side Pooling Engine

**File**: `R/pooling.R` (792 lines)

The pooling engine aggregates per-server results into a single "pooled" view. It implements mathematically correct formulas while respecting disclosure control (suppressed values become `NA`).

#### Policies

- **`"strict"`**: Only pool if ALL servers return valid data. Any NA/error causes the entire pooled cell to be NA. This prevents "pooling to unmask" — where combining multiple small-count sites could reveal suppressed values.
- **`"pooled_only_ok"`** (best effort): Pool what's available, skip servers with NA/errors.

#### Pooling Formulas

| Function | Formula | Use Case |
|---|---|---|
| `.pool_counts()` | `sum(vals)` | Row counts, person counts |
| `.pool_means()` | `sum(n_i * mean_i) / sum(n_i)` | Weighted mean across sites |
| `.pool_variance()` | Cochrane: `(within_SS + between_SS) / (N - 1)` where `within_SS = sum((n_i-1)*var_i)`, `between_SS = sum(n_i*(mean_i - pooled_mean)^2)` | Pooled standard deviation |
| `.pool_proportions()` | `sum(numerators) / sum(denominators)` | Missingness rates |
| `.pool_histograms()` | Bin-wise sum (assumes identical bin edges) | Numeric distributions |
| `.pool_top_k()` | Two-pass merge: union concept IDs across servers, sum metric, re-rank, take top K | Concept prevalence |

#### Dispatcher: `.pool_result(per_site, result_type, policy)`

Central switch that routes each result type to the appropriate pooling strategy:

| Result Type | Strategy |
|---|---|
| `table_stats` | Sum rows + sum persons |
| `column_stats` | Pool N, weighted mean, Cochrane variance |
| `domain_coverage` | Sum n_records/n_persons per table |
| `missingness` | Sum n_missing / sum n_total per column |
| `value_counts` | Top-K merge by value |
| `concept_prevalence` | Top-K merge by concept_id |
| `histogram` | Bin-wise sum |
| `date_counts` | Adapt to histogram format, then bin-sum |
| `concept_drilldown` | Composite: pool summary counts + pool histograms + pool categorical values; skip quantiles (not poolable) |
| `concept_locate` | Group by (concept_id, table_name, concept_column), sum counts |
| `achilles_results` | Group by (analysis_id, strata), sum count_value |
| `achilles_distribution` | Weighted mean + Cochrane SD + global min/max; median and percentiles set to NA (not poolable from summaries) |

---

## 4. Data Access Layer

All functions in this layer follow a consistent pattern:

```r
ds.omop.FUNCTION <- function(params..., scope, pooling_policy, symbol, conns, execute) {
  scope <- match.arg(scope)
  code <- .build_code(...)                      # Generate reproducible R code
  if (!execute) return(dry_run_result)           # Dry-run mode
  session <- .get_session(symbol)                # Get session handle
  raw <- DSI::datashield.aggregate(conns, ...)   # Execute on all servers
  pooled <- if (scope == "pooled") .pool_result(raw, type, policy) else NULL
  dsomop_result(per_site = raw, pooled, meta)    # Wrap in standard result
}
```

### 4.1 `dictionary.R` — Schema Browsing

**File**: `R/dictionary.R` (159 lines)

Schema discovery functions that inspect the CDM structure on remote servers.

| Function | Server Call | Returns |
|---|---|---|
| `ds.omop.tables(schema_category)` | `omopListTablesDS` | Per-server data.frame with table_name, schema_category, has_person_id, n_columns |
| `ds.omop.columns(table)` | `omopListColumnsDS` | Per-server data.frame with column_name, data_type, concept_role |
| `ds.omop.joins()` | `omopRelationshipGraphDS` | Per-server data.frame of table relationships (edges) |
| `ds.omop.compare()` | (client-side logic) | Cross-server schema diff: common_tables, server_only, column_diffs |
| `ds.omop.snapshot()` | `omopGetCapabilitiesDS` + `omopRelationshipGraphDS` | Full schema snapshot per server (tables, cdm_info, edges) |

**`ds.omop.compare()`** is notable because it operates entirely client-side. It compares the `capabilities` metadata from each server to find:
- Tables common to all servers
- Tables unique to specific servers
- Column differences for common tables

---

### 4.2 `profiling.R` — Safe Data Summary Wrappers

**File**: `R/profiling.R` (261 lines)

Functions for safely summarizing data without exposing individual-level information.

| Function | Server Call | Returns |
|---|---|---|
| `ds.omop.table.stats(table, stats)` | `omopTableStatsDS` | Row count, person count (per server or pooled) |
| `ds.omop.column.stats(table, column)` | `omopColumnStatsDS` | Count, mean, SD, min, max with suppression |
| `ds.omop.domain.coverage()` | `omopDomainCoverageDS` | Per-table n_records + n_persons |
| `ds.omop.missingness(table, columns)` | `omopMissingnessDS` | column_name, n_total, n_missing, missing_rate |
| `ds.omop.value.counts(table, column, top_n)` | `omopValueCountsDS` | Top-N value frequencies |

All return `dsomop_result` objects and support `scope = "pooled"` via the pooling engine.

---

### 4.3 `exploration.R` — Exploration Wrappers

**File**: `R/exploration.R` (392 lines)

Higher-level exploration functions used primarily by OMOP Studio.

| Function | Server Call | Description |
|---|---|---|
| `ds.omop.safe.cutpoints(table, column, ...)` | `omopSafeCutpointsDS` | Safe bin edges for numeric columns (each bin has enough persons for disclosure) |
| `ds.omop.concept.prevalence(table, ...)` | `omopConceptPrevalenceDS` | Top concepts ranked by person count or record count |
| `ds.omop.value.histogram(table, value_col, ...)` | `omopNumericHistogramDS` | Binned numeric distribution |
| `ds.omop.value.quantiles(table, value_col, ...)` | `omopNumericQuantilesDS` | Quantile values (NOT poolable — warning emitted) |
| `ds.omop.date.counts(table, date_col, ...)` | `omopDateCountsDS` | Temporal distribution (year/quarter/month) |
| `ds.omop.concept.drilldown(table, concept_id, ...)` | `omopConceptDrilldownDS` | Full concept profile: summary, numeric dist, categorical, dates, missingness |
| `ds.omop.concept.locate(concept_ids, ...)` | `omopLocateConceptDS` | Presence matrix: where do these concepts appear across all CDM tables? |

**`ds.omop.concept.drilldown`** is the most complex — it returns a composite structure:
```r
# per_site[[server]] = list(
#   summary = list(n_records, n_persons),
#   numeric_summary = list(histogram, quantiles),
#   categorical_values = data.frame(value_as_concept_id, concept_name, n),
#   date_coverage = data.frame(period, n_records),
#   missingness = data.frame(column_name, n_total, n_missing, missing_rate)
# )
```

---

### 4.4 `vocabulary.R` — Vocabulary Operations

**File**: `R/vocabulary.R` (178 lines)

Functions for searching and navigating the OMOP vocabulary.

| Function | Server Call | Description |
|---|---|---|
| `ds.omop.concept.search(pattern, domain, ...)` | `omopSearchConceptsDS` | Full-text search by name pattern with domain/vocabulary/standard filters |
| `ds.omop.concept.lookup(concept_ids)` | `omopLookupConceptsDS` | Lookup concepts by exact IDs |
| `ds.omop.concept.descendants(ancestor_ids, ...)` | `omopGetDescendantsDS` | Get descendants via concept_ancestor table |
| `ds.omop.concept.set(concepts, ...)` | (client-only) | Build concept set spec (include_descendants, include_mapped, exclude) |
| `ds.omop.concept.expand(concept_set)` | `omopExpandConceptSetDS` | Expand concept set to full ID list on server |

**Note**: `ds.omop.concept.set()` is purely client-side — it creates a specification object that can be sent to the server for expansion. Vocabulary searches return `dsomop_result` objects without pooling (vocabulary is metadata, not patient data).

---

### 4.5 `catalog.R` — Catalog Query System

**File**: `R/catalog.R` (258 lines)

The catalog is a repository of pre-built query templates on the server (similar to OHDSI's WebAPI). Each query has inputs, outputs, a mode (aggregate/assign), and disclosure safety classification.

| Function | Server Call | Description |
|---|---|---|
| `ds.omop.catalog.list(domain, provider)` | `omopCatalogListDS` | List available queries with metadata (id, group, name, mode, class, poolable) |
| `ds.omop.catalog.get(query_id)` | `omopCatalogGetDS` | Get full query details (inputs, output schema, sensitive fields) |
| `ds.omop.catalog.exec(query_id, inputs, mode)` | `omopCatalogExecDS` | Execute query; aggregate mode returns data, assign mode creates server-side table |
| `ds.omop.catalog.pool(results, query_id, ...)` | (client-side) | Pool catalog results with merge-based strategy |

#### Catalog Pooling (`ds.omop.catalog.pool`)

The catalog pooling function is more sophisticated than the standard pooling engine because it handles arbitrary query schemas:

```
1. Filter out non-data.frame results (errors)
2. If single server → return as-is
3. Auto-detect sensitive fields from query metadata or column name patterns (n_, _count, count_value, num_)
4. Determine join keys (non-sensitive, non-numeric columns + *_id columns)
5. Merge data frames by join keys
6. Pool sensitive columns using .pool_col():
   - strict: NA if ANY server suppressed → NA
   - pooled_only_ok: sum available values
```

---

### 4.6 `achilles.R` — Achilles Pre-Computed Statistics

**File**: `R/achilles.R` (205 lines)

[Achilles](https://github.com/OHDSI/Achilles) is an OHDSI tool that pre-computes aggregate statistics about a CDM database. These functions query the Achilles results tables.

| Function | Server Call | Description |
|---|---|---|
| `ds.omop.achilles.status()` | `omopAchillesStatusDS` | Check if Achilles tables exist on each server |
| `ds.omop.achilles.analyses(domain)` | `omopAchillesAnalysesDS` | List available analyses (catalog is identical across servers → pooled picks first) |
| `ds.omop.achilles.results(analysis_ids, stratum_filters, ...)` | `omopAchillesResultsDS` | Count results with optional stratum filtering |
| `ds.omop.achilles.distribution(analysis_ids, ...)` | `omopAchillesDistributionDS` | Distribution results (avg, stdev, min, max, percentiles) |
| `ds.omop.achilles.catalog()` | `omopAchillesCatalogDS` | Full analysis catalog |

---

## 5. Plan & Cart System

### 5.1 `plan.R` — Extraction Plan DSL

**File**: `R/plan.R` (598 lines)

The plan is a declarative specification of what to extract from the CDM. It describes a cohort (who), outputs (what), and options (how).

#### Plan Structure

```r
plan <- list(
  cohort = NULL,               # Cohort filter (cohort_table or spec)
  anchor = list(               # Base table for joins
    table = "person",
    id_col = "person_id"
  ),
  outputs = list(),            # Named list of output specifications
  options = list(
    translate_concepts = FALSE,
    block_sensitive = TRUE,
    min_persons = NULL
  )
)
class(plan) <- c("omop_plan", "list")
```

#### Plan Builder Functions (fluent API)

All return the modified plan for chaining:

```r
plan <- ds.omop.plan()                                # Create empty plan
plan <- ds.omop.plan.cohort(plan, cohort_definition_id = 1)   # Set cohort
plan <- ds.omop.plan.baseline(plan, columns, derived)          # Demographics
plan <- ds.omop.plan.person_level(plan, tables)                # Raw table join
plan <- ds.omop.plan.events(plan, name, table, ...)            # Event extraction
plan <- ds.omop.plan.features(plan, name, table, specs)        # Feature engineering
plan <- ds.omop.plan.survival(plan, outcome_table, ...)        # Time-to-event
plan <- ds.omop.plan.outcome(plan, name, concept_set, table)   # Outcome convenience
plan <- ds.omop.plan.cohort_membership(plan, name)             # OHDSI cohort table
plan <- ds.omop.plan.intervals(plan, tables, ...)              # Interval data
plan <- ds.omop.plan.temporal_covariates(plan, table, ...)     # Time-binned covariates
plan <- ds.omop.plan.options(plan, translate_concepts, ...)    # Plan-wide options
```

#### Output Types

| Type | Function | Description |
|---|---|---|
| `baseline` | `plan.baseline()` | One row per person from person table + derived fields (age_at_index, etc.) |
| `person_level` | `plan.person_level()` | Wide join of multiple tables by person_id |
| `event_level` | `plan.events()` | Rows of events with temporal/date handling; format: "long", "features" |
| `survival` | `plan.survival()` | Time-to-event with event indicator (0/1) and time in days |
| `concept_dictionary` | `plan.concept_dictionary()` | Lookup table for concept IDs referenced in other outputs |
| `cohort_membership` | `plan.cohort_membership()` | Standard OHDSI cohort table |
| `intervals_long` | `plan.intervals()` | Start/end days relative to index date |
| `temporal_covariates` | `plan.temporal_covariates()` | FeatureExtraction-compatible sparse time-binned covariates |

#### Temporal & Date Handling Helpers

```r
omop.temporal(index_window, calendar, event_select, min_gap)
# index_window = list(start = -365, end = 0)  → events within 365 days before index
# event_select = list(order = "first", n = 1)  → take first event only

omop.date_handling(mode, reference, bin_width, date_columns)
# mode: "absolute" | "relative" | "binned" | "remove"
```

#### Plan Execution

```r
ds.omop.plan.validate(plan, symbol)   # Validate against server schemas
ds.omop.plan.preview(plan, symbol)    # Safe aggregate preview
ds.omop.plan.execute(plan, out, symbol) # Execute and create server-side tables
ds.omop.plan.harmonize(plan, mode)    # Adjust plan for multi-server compatibility
```

**`plan.execute()`** sends the plan to the server, which assigns each output as a named symbol in the DataSHIELD session. The `out` parameter maps output names to symbol names:

```r
ds.omop.plan.execute(plan, out = c(baseline = "D_base", survival = "D_tte"))
# Creates D_base and D_tte as server-side data frames
```

---

### 5.2 `cart.R` — Cart / Recipe Infrastructure

**File**: `R/cart.R` (1389 lines)

The cart is the highest-level abstraction for defining an OMOP extraction recipe. It's the "shopping basket" paradigm: browse concepts, add them to the cart, then compile to a plan for execution.

#### Architecture

```
Cart
├── Populations (DAG)
│   ├── base (root, always exists)
│   └── subpop_1 (parent → base, with filter chain)
├── Variable Blocks (grouped variables)
│   └── block_measurement_5 → {concept_ids, table, format, time_window, filters}
├── Variables (individual)
│   └── diabetes → {table, concept_id, format, value_source}
├── Filters
│   ├── f1_sex (population-level)
│   ├── f2_age_range (population-level)
│   └── f3_AND [group]
│       ├── has_concept (population-level)
│       └── date_range (row-level)
└── Outputs
    ├── main_output [wide] pop:base
    └── events_output [long] pop:subpop_1
```

#### Naming Engine

The naming engine auto-generates safe R variable names from concept names:

```r
.sanitize_name("Type 2 Diabetes Mellitus")  # → "type_2_diabetes_mellitus"
.ensure_unique_name("diabetes", c("diabetes"))  # → "diabetes_2"
.suffix_names("hba1c", 3, mode = "index")  # → c("hba1c_1", "hba1c_2", "hba1c_3")
```

#### Core Constructors

**`omop_variable()`** — What to extract:
```r
omop_variable(
  name = "hba1c",                    # Output column name (auto-generated if NULL)
  table = "measurement",             # Source OMOP table
  column = NULL,                     # Source column
  concept_id = 3004410,              # Concept filter
  concept_name = "HbA1c",           # Human-readable name
  type = "auto",                     # auto|numeric|categorical|date|boolean|integer|character
  format = "raw",                    # raw|binary|count|first_value|last_value|mean|min|max|time_since|binned
  value_source = "value_as_number", # Column to extract values from
  time_window = list(start=-365, end=0),  # Relative to index date
  filters = list()                   # Variable-specific filters
)
```

**`omop_filter()`** — Constraints at three levels:
- **population**: Person-level restrictions (sex, age, cohort, has_concept)
- **row**: Event-level restrictions (date_range, value_threshold, concept_set)
- **output**: Post-extraction restrictions (min_count, top_n, dedup)

12 filter types: `sex`, `age_range`, `age_group`, `cohort`, `has_concept`, `date_range`, `value_threshold`, `concept_set`, `min_count`, `top_n`, `dedup`, `custom`.

Convenience constructors: `omop_filter_sex("F")`, `omop_filter_age(min=18, max=65)`, `omop_filter_has_concept(201826, "condition_occurrence")`, `omop_filter_date_range(start="2020-01-01")`, `omop_filter_value(">", 7.0)`.

**`omop_filter_group()`** — Nested AND/OR logic:
```r
omop_filter_group(
  omop_filter_sex("F"),
  omop_filter_age(min = 18, max = 65),
  operator = "AND"
)
```

**`omop_population()`** — DAG node for subpopulations:
```r
omop_population(id = "females", label = "Female subset", parent_id = "base",
                filters = list(omop_filter_sex("F")))
```

**`omop_variable_block()`** — Groups variables sharing table/window/filters. When added to the cart, each concept_id is expanded into an individual `omop_variable`.

**`omop_output()`** — Output shape specification:
8 types: `wide`, `long`, `features`, `survival`, `intervals`, `baseline`, `joined_long`, `covariates_sparse`.

**`omop_cart()`** — The cart itself:
```r
omop_cart()  # Creates empty cart with base population
```

#### Cart Mutators

All return the modified cart (immutable-style):

| Function | Description |
|---|---|
| `cart_add_variable(cart, variable)` | Add variable (ensures unique name) |
| `cart_add_block(cart, block)` | Add block + expand concepts to individual variables |
| `cart_add_filter(cart, filter, id)` | Add filter (auto-generates ID if NULL) |
| `cart_add_population(cart, population)` | Add population node (validates parent exists) |
| `cart_add_output(cart, output)` | Add output specification |
| `cart_remove_variable/filter/population/output(cart, id)` | Remove by ID |
| `cart_clear(cart)` | Reset to empty cart |

#### Cart → Plan Conversion (`cart_to_plan`)

The core compiler that translates a cart into an executable plan:

```
1. Create empty plan
2. If base population has cohort_definition_id → set plan cohort
3. Collect population-level filters → add to cohort spec
4. For each output:
   a. Get referenced variables (output.variables or all)
   b. Group variables by source table
   c. Based on output type:
      - "wide"/"baseline" → plan.baseline() or plan.person_level()
      - "long"/"joined_long" → plan.events() per table
      - "features"/"covariates_sparse" → plan.features() per table
        (maps variable format → feature spec: binary→boolean, count→count, etc.)
      - "survival" → plan.survival() with concept_ids from variables
      - "intervals" → plan.intervals() with tables from variables
5. Collect row-level filters → apply to relevant outputs
6. Return plan
```

#### Cart ↔ Code / JSON

```r
cart_to_code(cart)          # Generate minimal R code that recreates the cart
cart_export_json(cart, file) # Export to JSON (strips S3 classes for clean serialization)
cart_import_json(json)       # Import from JSON string or file path
```

#### Cart Execution

```r
cart_preview_schema(cart)    # Preview output columns without executing (client-only)
cart_preview_stats(cart, scope, symbol)  # Run aggregate-only queries for stats
cart_execute(cart, out, symbol)  # Compile to plan + execute
```

#### Filter Safety Classification

`.classifyFilterClient(filter_type)` mirrors the server-side safety classification for UI display:
- **allowed**: sex, age_group, cohort, concept_set
- **constrained**: age_range, has_concept, date_range, min_count
- **blocked**: value_threshold, custom

---

### 5.3 `feature.R` — Feature Recipe Helpers

**File**: `R/feature.R` (162 lines)

Each function returns an `omop_feature_spec` object for use in `ds.omop.plan.features()`:

| Function | Type | Description |
|---|---|---|
| `omop.feature.boolean(concept_set)` | `"boolean"` | Binary presence/absence (0/1) |
| `omop.feature.count(concept_set)` | `"count"` | Number of events per person |
| `omop.feature.latest_value(concept_set, value_column)` | `"latest_value"` | Most recent value_as_number |
| `omop.feature.first_value(concept_set, value_column)` | `"first_value"` | First recorded value |
| `omop.feature.time_since(concept_set, reference_date, unit)` | `"time_since"` | Days/months since event |
| `omop.feature.mean_value(concept_set, value_column)` | `"mean_value"` | Mean across events |
| `omop.feature.min_value(concept_set, value_column)` | `"min_value"` | Minimum value |
| `omop.feature.max_value(concept_set, value_column)` | `"max_value"` | Maximum value |

---

### 5.4 `cohort.R` — Cohort Management

**File**: `R/cohort.R` (126 lines)

| Function | Server Call | Description |
|---|---|---|
| `ds.omop.cohort.list()` | `omopCohortListDS` | List available cohort definitions |
| `ds.omop.cohort.definition(id)` | `omopCohortGetDefinitionDS` | Get cohort definition details |
| `ds.omop.cohort.create(spec, mode, ...)` | `omopCohortCreateDS` | Create cohort from spec (temporary or persistent) |
| `ds.omop.cohort.ref(cohort_definition_id)` | (client-only) | Create cohort reference for plan DSL |
| `ds.omop.cohort.combine(op, cohort_a, cohort_b, ...)` | `omopCohortCombineDS` | Set operations: intersect, union, setdiff |

**`ds.omop.cohort.create`** requires a `spec` with `type` and `concept_set` fields. The server creates a temporary table that can be referenced by the plan.

---

## 6. OMOP Studio (Shiny Application)

### 6.1 `studio.R` — Main App + Shared Utilities

**File**: `R/studio.R` (423 lines)

#### Entry Point: `ds.omop.studio(symbol)`

```r
ds.omop.studio <- function(symbol = "omop", launch.browser = TRUE) {
  app <- shiny::shinyApp(ui = .studio_ui(symbol), server = .studio_server(symbol))
  shiny::runApp(app, launch.browser = launch.browser)
}
```

#### UI: `.studio_ui(symbol)`

Creates a `bslib::page_navbar` with 11 tabs:

| Tab # | Title | Icon | Module ID | UI Function |
|---|---|---|---|---|
| 1 | Connections | plug | `conn` | `.mod_connections_ui` |
| 2 | Explore | compass | `explore` | `.mod_table_concepts_ui` |
| 3 | Drilldown | magnifying-glass-chart | `drilldown` | `.mod_concept_drilldown_ui` |
| 4 | Locator | location-dot | `locator` | `.mod_concept_locator_ui` |
| 5 | Vocabulary | book | `vocab` | `.mod_vocab_ui` |
| 6 | Data Sources | chart-bar | `atlas` | `.mod_atlas_ui` |
| 7 | Catalog | book-open | `catalog` | `.mod_catalog_ui` |
| 8 | Basket | cart-shopping | `basket` | `.mod_basket_ui` |
| 9 | Plan Builder | hammer | `plans` | `.mod_plan_from_explorer_ui` |
| 10 | Script | code | `script` | `.mod_script_builder_ui` |
| 11 | Session | server | `session_tab` | `.mod_session_ui` |

Theme: Bootstrap 5 / Flatly with custom CSS for metric cards, suppressed values, code output, cart items, server badges, etc.

#### Server: `.studio_server(symbol)`

Creates shared `state` (a `shiny::reactiveValues` object) accessible by all modules:

```r
state <- shiny::reactiveValues(
  symbol              = symbol,         # OMOP session symbol
  status              = NULL,           # ds.omop.status() result
  tables              = NULL,           # ds.omop.tables() result
  selected_table      = NULL,           # Currently selected table name
  selected_concept_col = NULL,          # Currently selected concept column
  selected_concept_id  = NULL,          # Currently selected concept ID
  selected_concept_name = NULL,         # Currently selected concept name
  concept_set         = integer(0),     # Accumulated concept IDs
  cart                = omop_cart(),     # Shopping cart
  plan                = ds.omop.plan(), # Extraction plan
  plan_outputs        = list(),         # Plan output mappings
  script_lines        = character(0),   # Accumulated R script lines
  scope               = "per_site",     # Current scope setting
  pooling_policy      = "strict",       # Current pooling policy
  server_names        = character(0)    # Connected server names
)
```

On startup, fetches `ds.omop.status()` and `ds.omop.tables()`, populating `state$server_names`.

#### Multi-Server Shared Utilities

**`.scope_controls_ui(ns, default, show_pooled)`** — Reusable UI component with:
- Radio buttons: All Servers / Per Site / Pooled (optional)
- `selectizeInput` for multi-select server names
- Checkbox: "Intersection only (common to selected)"
- Conditional pooling policy radio (strict/best effort)

**`.server_selector_ui(ns)`** — Simplified version (no scope radio) for modules that only need server selection.

**`.scope_sync_servers(input, session, state)`** — Observer that populates the server dropdown from `state$server_names`. All servers are selected by default.

**`.backend_scope(scope)`** — Maps UI scope to backend scope:
- `"all"` → `"per_site"` (the backend doesn't know about "all" — it's a client-side rbind)
- Others pass through

**`.resolve_servers(per_site, selected_server)`** — Filters available server names to only those selected in the UI dropdown.

**`.extract_display_data(res, scope, selected_server, intersect_only, intersect_col, server_col)`** — Central utility for extracting display data from `dsomop_result`:
```
scope = "pooled" → return res$pooled
scope = "per_site" → return per_site[[first selected server]]
scope = "all" → rbind all selected servers with added server column
  if intersect_only → keep only rows where intersect_col appears in ALL servers
```

**Other helpers**: `.get_person_tables()`, `.get_concept_columns()`, `.domain_to_table()`, `.format_table_name()`, `.table_choices()`, `.parse_ids()`, `.fmt_count()`, `.safe_plot()`.

---

### 6.2 `studio_connections.R` — Module 1: Connections

**File**: `R/studio_connections.R` (157 lines)

Displays the status of connected servers.

**UI**: Three cards:
1. **Server Status** — Per-server card with connection status (green/red), database name, DBMS type, table count
2. **Dataset Identity (CDM_SOURCE)** — CDM source metadata per server (source_name, abbreviation, CDM version, vocabulary version)
3. **Disclosure Settings** — DataSHIELD privacy options (nfilter.tab, nfilter.subset, nfilter.levels.max, privacy level)

**Server logic**: Refresh button calls `ds.omop.status()` and `ds.omop.tables()`.

---

### 6.3 `studio_explore.R` — Module 2: Explore

**File**: `R/studio_explore.R` (367 lines)

The primary concept exploration interface. Users select a table, view top concepts, and interact with them.

**UI (sidebar)**:
- Table dropdown (populated from `state$tables`)
- Concept column dropdown (populated when table changes)
- Metric selector (persons/records)
- Top N slider
- Suppression toggle
- Run button
- Scope controls (`.scope_controls_ui`)
- Concept Set display + Clear Set button

**UI (main area)**:
- Table Stats card (row count, person count)
- Concept Prevalence table with action buttons:
  - Add to Set — adds selected concept IDs to `state$concept_set`
  - +Extract — creates `omop_variable` from selected concepts and adds to cart
  - +Filter — creates `omop_filter_has_concept` and adds to cart
  - Row click → navigates to Drilldown tab

**Server logic**:
```
1. On table change → fetch columns via ds.omop.columns()
2. On Run → ds.omop.concept.prevalence(table, concept_col, metric, top_n, scope, ...)
3. Store raw result in raw_prevalence_result()
4. Extract display data via .extract_display_data(res, scope, selected_server, intersect_only)
5. Apply suppression filter if enabled
6. On scope/server/intersect change → re-extract from raw (no re-query)
7. On row click → update state$selected_*, navigate to Drilldown
```

---

### 6.4 `studio_drilldown.R` — Module 3: Drilldown

**File**: `R/studio_drilldown.R` (497 lines)

Deep-dive profile for a single concept. Shows all available data about how a concept appears in the CDM.

**UI (sidebar)**:
- Table + Concept ID inputs (auto-populated from Explore tab navigation)
- Load Profile button + Reload button
- Server selector (`.server_selector_ui`)

**UI (main area)**: Five cards:
1. **Summary** — Metric cards: n_records, n_persons, records per person (with multi-server comparison when multiple servers selected)
2. **Numeric Distribution** — Histogram plot
3. **Categorical Values** — Bar chart + table of value counts
4. **Date Coverage** — Line chart of record counts over time
5. **Missingness** — Bar chart + table of missing rates per column

**Server logic**:
```
1. On Load Profile → ds.omop.concept.drilldown(table, concept_id, scope, ...)
2. Store full result in drilldown_data()
3. Helper: .drilldown_pick_server(dd, selected_server) → uses .resolve_servers()
   For multi-server: show comparison grid in summary card
4. On server change → re-extract from stored result (no re-query)
5. Plots use .safe_plot() wrapper to handle errors gracefully
```

---

### 6.5 `studio_locator.R` — Module 4: Concept Locator

**File**: `R/studio_locator.R` (215 lines)

Searches all CDM tables for given concept IDs and shows a presence matrix.

**UI (sidebar)**:
- Concept IDs text input (comma-separated)
- Name search (mini vocabulary search with DT table)
- Locate button
- Scope controls (`.scope_controls_ui` with default="all")

**Server logic**:
```
1. Auto-populate from state$selected_concept_id
2. Name search → ds.omop.concept.search(term, standard_only=TRUE, limit=20)
3. Selected search results → append to concept IDs field
4. On Locate → ds.omop.concept.locate(ids, symbol)
5. Store raw result; extract via .locator_extract(res, scope, selected_server, intersect_only)
6. On scope change → re-extract
```

**`.locator_extract()`** handles three modes:
- **all**: rbind per-server data with `server` column
- **per_site**: show first selected server only
- **pooled**: aggregate n_records/n_persons by (table_name, concept_column, concept_id)

---

### 6.6 `studio_vocab.R` — Module 5: Vocabulary Browser

**File**: `R/studio_vocab.R` (206 lines)

Full vocabulary search with concept set management.

**UI (sidebar)**:
- Search text + domain filter + standard-only + limit
- Search button
- Server selector (`.server_selector_ui`)
- Concept Set display + Clear Set button

**UI (main area)**:
- Search Results table (selectable, multi-select) with:
  - "Add to Set" button — adds selected concept IDs to `state$concept_set`
  - "+Extract" button — creates `omop_variable` from each selected concept and adds to cart
- Concept Details card (shows first selected row's full metadata)

**Server logic**:
```
1. On Search → ds.omop.concept.search(pattern, domain, standard_only, limit, ...)
2. Store raw result; extract via .extract_display_data(res, "all", selected_server, intersect_only)
3. On server/intersect change → re-extract
4. Table columns: concept_id, concept_name, domain_id, vocabulary_id, standard_concept, server
5. +Extract creates omop_variable with table inferred from domain (.domain_to_table)
```

---

### 6.7 `studio_atlas.R` — Module 6: Data Sources (Atlas)

**File**: `R/studio_atlas.R` (1057 lines)

The largest module. Provides an Achilles-powered data source characterization dashboard, similar to OHDSI Atlas "Data Sources" tab.

**UI**: Sidebar with:
- Scope controls
- Dynamic navigation buttons generated from the Achilles analysis catalog

**Main area**: Dynamic content rendered based on selected navigation item. Sections include:
- **Dashboard** — Person count, observation period, age/gender distributions
- **Person** — Demographics breakdown (age, gender, race, ethnicity)
- **Domain pages** (Condition, Drug, Measurement, Procedure, Observation, Visit, Device) — Concept prevalence, age distributions, type concepts
- **Visits** — Visit type, duration, age at first visit
- **Observation Periods** — Duration, cumulative, age at first observation
- **Death** — Death by cause, type, age
- **Trends** — Temporal trends (records per person by year)
- **Quality** — Data quality completeness

**Server logic**:
```
1. On startup → ds.omop.achilles.status() to check availability
2. Load analysis catalog → ds.omop.achilles.catalog()
3. Generate navigation buttons dynamically from catalog
4. On navigation click:
   a. Fetch data via ds.omop.achilles.results() or ds.omop.achilles.distribution()
   b. Store raw result; extract via .atlas_pick_result(res, scope, selected_server)
   c. Render plots (barplot, pie chart, histogram) and tables
5. On scope/server change → re-extract from cached results

.atlas_pick_result(res, scope, selected_server):
  - "all" → rbind per-server data with server column (via .resolve_servers)
  - "per_site" → first selected server
  - "pooled" → res$pooled
```

---

### 6.8 `studio_catalog.R` — Module 7: Catalog

**File**: `R/studio_catalog.R` (438 lines)

Interactive interface for the query template catalog.

**UI (sidebar)**:
- Domain filter dropdown
- Query list (DT table, selectable)
- Scope controls

**UI (main area)**:
- Query Details card (description, mode, inputs, outputs)
- Dynamic Parameter Form (auto-generated from query metadata)
  - Concept ID input with search helper
  - Text, numeric, select inputs based on parameter types
- Execute button → Results table with pooling

**Server logic**:
```
1. On load → ds.omop.catalog.list() to populate query list
2. On query select → ds.omop.catalog.get(query_id) to show details
3. Auto-generate parameter form from query inputs
4. On Execute → ds.omop.catalog.exec(query_id, inputs, mode)
5. Store raw result; on scope change → re-extract or re-pool
6. If pooled → ds.omop.catalog.pool(results, query_id, ...)
```

---

### 6.9 `studio_basket.R` — Module 8: Basket (Cart UI)

**File**: `R/studio_basket.R` (856 lines)

The full cart management interface — the central authoring tool.

**UI**: Two-column layout:
- **Left panel** — Cart contents:
  - Populations section (tree display with add/remove)
  - Variables section (each with remove/edit buttons)
  - Filters section (with remove buttons, shows level and label)
  - Outputs section (with remove buttons, shows type and population)
- **Right panel** — Actions:
  - Quick Add Variable (table, concept search, format)
  - Quick Add Block (table, concept search, format → bulk add)
  - Quick Add Filter (type → dynamic params → auto-detect level)
  - Quick Add Output (name, type, population, variables)
  - JSON Import/Export
  - Schema Preview (table showing projected columns per output)
  - Generated Code (syntax-highlighted R code from `cart_to_code()`)

**Server logic**:
```
1. Cart state is shared via state$cart
2. Each "Add" action modifies state$cart immutably (replace with returned cart)
3. Quick Add Variable:
   a. Mini concept search via ds.omop.concept.search()
   b. Select from DT results → build omop_variable → cart_add_variable
4. Quick Add Block:
   a. Same search but multiple selection → build omop_variable_block → cart_add_block
5. Quick Add Filter:
   a. Dynamic UI based on filter type (sex → radio, age_range → sliders, etc.)
   b. Build appropriate omop_filter → cart_add_filter
6. Quick Add Output:
   a. Select type, name, population → build omop_output → cart_add_output
7. JSON Import: cart_import_json() → replace state$cart
8. JSON Export: cart_export_json(state$cart) → download
9. Schema Preview: cart_preview_schema(state$cart) → DT table
10. Code: cart_to_code(state$cart) → highlighted display
```

---

### 6.10 `studio_plan.R` — Module 9: Plan Builder

**File**: `R/studio_plan.R` (255 lines)

Interface for building and executing extraction plans.

**UI (sidebar)**:
- Concept Set display (from `state$concept_set`)
- Cohort management (list, create from concept set, select)
- Output type buttons: Baseline, Events, Features, Survival, Intervals, Temporal, Dictionary

**UI (main area)**:
- Plan Summary card (renders `print.omop_plan()` output)
- Execute Plan card (output name → symbol mapping, execute button)
- Generated Code card

**Server logic**:
```
1. Each output type button adds to state$plan using the plan DSL:
   - Baseline: ds.omop.plan.baseline(plan, ...)
   - Events: ds.omop.plan.events(plan, table, concept_set, ...)
   - Features: ds.omop.plan.features(plan, table, specs, ...)
   - Survival: ds.omop.plan.survival(plan, outcome_concepts, ...)
   - etc.
2. Execute: ds.omop.plan.execute(state$plan, out, ...)
3. Code generation: .studio_codegen_plan(plan, out, symbol)
```

---

### 6.11 `studio_script.R` — Module 10: Script

**File**: `R/studio_script.R` (152 lines)

Accumulates all R code generated by other modules into a reviewable, downloadable script.

**UI**: Single card with:
- Syntax-highlighted R code display
- Copy to Clipboard button (browser-native via `navigator.clipboard`)
- Download .R button
- Clear button

**Server logic**:
```
1. Reads state$script_lines (character vector)
2. Joins into single string; applies .highlightR() for syntax coloring
3. Copy: sends JS message via session$sendCustomMessage
4. Download: shiny::downloadHandler writes to .R file
5. Clear: resets state$script_lines
```

**`.highlightR(code)`** — Regex-based R syntax highlighter:
- Comments (`#...`) → `.r-comment`
- Strings (`"..."`, `'...'`) → `.r-string`
- Function calls (`name(`) → `.r-fn`
- Keywords (if, else, function, TRUE, FALSE, NULL, NA, ...) → `.r-keyword`
- Assignment (`<-`) → `.r-assign`
- Pipe operators (`|>`, `%>%`) → `.r-operator`
- Numbers → `.r-number`

---

### 6.12 `studio_session.R` — Module 11: Session

**File**: `R/studio_session.R` (164 lines)

Session information, domain coverage, and missingness explorer.

**UI**: Three cards:
1. **Remote Session Information** — Refresh button, server selector, session details (symbol, servers, errors)
2. **Domain Coverage** — Load button → table of n_records/n_persons per CDM table
3. **Missingness Explorer** — Table dropdown, Check button → missingness rates per column

**Server logic**:
```
1. .scope_sync_servers(input, session, state)
2. Refresh → ds.omop.status(symbol)
3. Coverage → ds.omop.domain.coverage(symbol)
   Store raw; extract via .extract_display_data(res, "all", selected_server, intersect_only)
   On server change → re-extract (no re-query)
4. Missingness → ds.omop.missingness(table, symbol)
   Same pattern: store raw, extract, re-extract on change
```

---

### 6.13 `studio_codegen.R` — Code Generation Helpers

**File**: `R/studio_codegen.R` (152 lines)

Generates reproducible R code from interactive Shiny state.

| Function | Purpose |
|---|---|
| `.studio_codegen_plan(plan, out, symbol)` | Generate full R script for a plan (cohort, outputs, execute) |
| `.studio_codegen_concept_set(concept_ids, name)` | Generate `ds.omop.concept.set(c(...))` code |
| `.studio_codegen_exploration(fn_name, args_list)` | Delegate to `.build_code()` for exploration functions |
| `.studio_codegen_profiling(fn_name, args_list)` | Delegate to `.build_code()` for profiling functions |
| `.studio_codegen_vocabulary(fn_name, args_list)` | Delegate to `.build_code()` for vocabulary functions |

**`.studio_codegen_plan`** handles all output types:
```r
# Generated code example:
plan <- ds.omop.plan()
plan <- ds.omop.plan.cohort(plan, cohort_definition_id = 1)
plan <- ds.omop.plan.baseline(plan, columns = c("gender_concept_id", "year_of_birth"),
                                derived = c("age_at_index"), name = "baseline")
plan <- ds.omop.plan.events(plan, name = "conditions", table = "condition_occurrence",
                             concept_set = c(201826, 4229440))
ds.omop.plan.execute(plan, out = c(baseline = "D_base", conditions = "D_cond"),
                      symbol = "omop")
```

---

## 7. Multi-Server Architecture

The multi-server architecture is a cross-cutting concern that affects all modules. Here is the unified pattern:

### Scope Model

```
┌─────────────────────────────────────────────────────┐
│  UI Scope: "all" | "per_site" | "pooled"            │
│                                                     │
│  "all"      → Client-side rbind with server column  │
│  "per_site" → Show single selected server           │
│  "pooled"   → Server-side pooled aggregation        │
└─────────────┬───────────────────────────────────────┘
              │
              ▼
┌─────────────────────────────────────────────────────┐
│  Backend Scope: "per_site" | "pooled"               │
│                                                     │
│  .backend_scope("all") → "per_site"                 │
│  Backend only knows per_site and pooled.             │
│  "all" is a client-side display concept.            │
└─────────────────────────────────────────────────────┘
```

### Standard Module Pattern

Every module that displays data from DSI follows this pattern:

```r
# In server setup:
.scope_sync_servers(input, session, state)     # Populate server dropdown
raw_result <- shiny::reactiveVal(NULL)          # Cache for raw dsomop_result

# After query execution:
res <- ds.omop.FUNCTION(..., scope = .backend_scope(input$scope), ...)
raw_result(res)                                 # Store raw result
display_data(.extract_display_data(res, input$scope %||% "all",
  input$selected_server, intersect_only = isTRUE(input$intersect_only)))

# On scope/server/intersect change (NO re-query):
shiny::observeEvent(list(input$scope, input$selected_server, input$intersect_only), {
  res <- raw_result()
  if (is.null(res)) return()
  display_data(.extract_display_data(res, input$scope, input$selected_server,
    intersect_only = isTRUE(input$intersect_only)))
}, ignoreInit = TRUE)
```

### Server Selection

- `selectizeInput` with `multiple = TRUE` — allows selecting any subset of servers
- Default: all servers selected
- `.resolve_servers(per_site, selected_server)` — maps selected names to available data
- "Intersection only" checkbox — when enabled, `rbind` result is filtered to only keep rows whose key (e.g., `concept_id`) appears in ALL selected servers

---

## 8. Data Flow Diagram

```
User interaction in OMOP Studio
         │
         ▼
┌──────────────────────┐
│  Shiny Module Server │  (e.g., studio_explore.R)
│                      │
│  1. Read UI inputs   │
│  2. Call API function │──────► ds.omop.concept.prevalence()
│  3. Store raw result │◄────── dsomop_result {per_site, pooled, meta}
│  4. Extract display  │
│  5. Render DT/plots  │
└──────────────────────┘
         │                                    │
         │ (scope change)                     │ (API call)
         │                                    ▼
         │                        ┌─────────────────────┐
         │                        │  exploration.R /    │
         │                        │  profiling.R / etc  │
         │                        │                     │
         │                        │  .get_session()     │
         │                        │  .build_code()      │
         │                        │  DSI::datashield.   │
         │                        │    aggregate()      │────► Remote Server
         │                        │  .pool_result()     │◄──── Per-site results
         │                        │  dsomop_result()    │
         │                        └─────────────────────┘
         │
         ▼
  .extract_display_data(res, scope, selected_server, intersect_only)
         │
         ├── "pooled"   → res$pooled
         ├── "per_site"  → res$per_site[[selected]]
         └── "all"       → rbind(all servers) + server column
                                + optional intersection filter
```

### Script Accumulation Flow

```
Any module operation that calls a ds.omop.* function
         │
         ├── Function returns dsomop_result with meta$call_code
         │
         ▼
  if (inherits(res, "dsomop_result") && nchar(res$meta$call_code) > 0)
    state$script_lines <- c(state$script_lines, res$meta$call_code)
         │
         ▼
  Script tab reads state$script_lines → .highlightR() → display
```

### Cart → Execution Flow

```
User builds cart in Basket tab
  (add variables, filters, populations, outputs)
         │
         ▼
  cart_to_plan(cart)     ← Compiler: cart structure → plan DSL
         │
         ▼
  ds.omop.plan.execute(plan, out)     ← Sends plan to server
         │
         ▼
  Server creates named data frames in DataSHIELD session
  (e.g., D_base, D_conditions, D_survival)
```

---

*This document was generated from an exhaustive traversal of the dsOMOPClient v2.0.0 codebase. All function signatures, internal logic, and architectural patterns are documented as they exist in the source code.*
