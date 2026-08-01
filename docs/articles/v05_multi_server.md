# Multi-Server Federated Analysis

## Overview

dsOMOP v2 is designed for federated analysis across multiple DataSHIELD
servers, each hosting an OMOP CDM database. Servers may differ in CDM
version, available tables and columns, vocabulary version, database
engine, disclosure policy and data volume. dsOMOP exposes those
differences through runtime introspection and negotiates a common
age/date/capacity policy. Baseline age groups are computed on the
intersection of public age boundaries, so sites with different but
coarsenable grids emit the same labels. Schema comparison does **not**
by itself make arbitrary heterogeneous plans portable: the analyst must
still reconcile the requested tables, columns, concepts and output
cardinality before execution.

## Connecting to Multiple Servers

``` r

library(dsOMOPClient)
library(DSI)
library(DSOpal)

builder <- DSI::newDSLoginBuilder()
builder$append(server = "hospital_a", url = "https://opal-a.example.org",
  user = "analyst", password = "secret_a", resource = "project.omop_cdm")
builder$append(server = "hospital_b", url = "https://opal-b.example.org",
  user = "analyst", password = "secret_b", resource = "project.omop_cdm")
builder$append(server = "hospital_c", url = "https://opal-c.example.org",
  user = "analyst", password = "secret_c", resource = "project.omop_cdm")

login_data <- builder$build()
conns <- DSI::datashield.login(logins = login_data)
```

## Attaching Resources

[`ds.omop.connect()`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.connect.md)
accepts a single resource name (applied to every server) or a named list
mapping servers to resources:

``` r

# Same resource on all servers
ds.omop.connect(resource = "project.omop_cdm", conns = conns, symbol = "omop")

# Or a per-server mapping
ds.omop.connect(
  resource = list(
    hospital_a = "project.omop_cdm",
    hospital_b = "project.omop_cdm",
    hospital_c = "project.omop_cdm"),
  conns = conns, symbol = "omop")
```

Connection setup is transactional across the requested federation. The
client requires one resource per server, rejects a pre-existing public
handle symbol, builds initialization as a literal call, verifies
capabilities everywhere, removes the transient resource symbols and only
then records the local session. Any partial resource assignment or
handle initialization closes/removes what was created before returning
an error.

## Schema Comparison

Before building a plan, compare schemas across servers with
[`ds.omop.compare()`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.compare.md):

``` r

comparison <- ds.omop.compare(symbol = "omop", conns = conns)

comparison$common_tables   # tables available on ALL servers
comparison$server_only     # tables unique to particular servers
comparison$column_diffs    # which columns are missing where
comparison$common_columns  # per-table columns available everywhere
comparison$common_column_types # canonical compatible SQL type families
comparison$column_type_diffs   # same name but incompatible type family
comparison$column_errors   # must be empty before harmonization
```

## Plan Harmonization

When schemas differ, inspect `comparison$column_diffs` and revise the
plan to a contract every server can execute.
[`ds.omop.plan.harmonize()`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.harmonize.md)
establishes that contract fail-closed: it refuses to claim harmonization
if column introspection failed on any common table.

### Intersection mode (default)

By default (`strict = TRUE`), every public plan output is checked:
person/event tables, baseline, survival, concept dictionaries,
intervals, temporal covariates and person-period panels. The contract
includes compatible SQL type families and implicit dependencies used by
feature specs, concept/date/visit filters and population criteria. For a
long or wide event request that leaves `columns = NULL`, a schema
difference is rejected because the server-selected default columns
cannot be proven identical. Unknown output types are rejected. This
makes portability explicit rather than silently changing the analysis.

With `strict = FALSE`, raw optional columns/tables are intersected with
aliases preserved. An output is removed as a unit when a required
feature, temporal or other semantic dependency is absent.
Population/cohort criteria are never weakened and therefore still fail
closed. Inspect the returned plan before execution: this mode
deliberately changes the requested output contract.

The returned plan records the exact servers and relevant schema
snapshot. `validate`, `preview` and `execute` recheck that binding;
modifying the plan, changing the selected nodes or changing a relevant
table/column/type requires calling
[`ds.omop.plan.harmonize()`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.harmonize.md)
again.

With two or more servers, `validate`, `preview` and `execute`
automatically run strict harmonization when an unbound plan reaches
them. Calling
[`ds.omop.plan.harmonize()`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.harmonize.md)
explicitly, as below, is still recommended when the analyst wants to
inspect the negotiated contract before execution.

A federated wide output is a closed schema contract. It must declare an
integer `concept_set` and set `translate_concepts = FALSE`; every
declared concept gets the same concept-ID-derived column on every node,
including an all-`NA` column when that concept is locally absent, and
undeclared concepts cannot enter the output. The request must also
reduce or deterministically select duplicate events so there is at most
one value per grain/concept.

``` r

plan <- ds.omop.plan()
plan <- ds.omop.plan.person_level(plan,
  tables = list(
    person = c("gender_concept_id", "race_concept_id"),
    death = c("cause_concept_id")))

# Strict harmonization either proves this request portable or errors.
plan <- ds.omop.plan.harmonize(plan, mode = "intersection",
  symbol = "omop", conns = conns)
```

### Unsupported union-with-missing mode

`"union_with_missing"` is rejected explicitly: the package does not
pretend an unchanged plan has been harmonized. Synthesizing typed
missing columns across all supported SQL backends needs an executable
contract before this strategy can be offered.

## Status Checking

``` r

status <- ds.omop.status(symbol = "omop")
# Per-server connection/handle status
```

## Federation-Wide Concept Factors

When a plan executes in memory mode with `factor_concepts = TRUE` (the
default), dsOMOP runs a cross-server coordination step after extraction:
it collects the permitted `_concept_id` levels, computes their union in
deterministic order and broadcasts that ordering so every server uses
the same codes. A value present on only some sites becomes an unused
level on the others.

This removes one common source of inconsistent model matrices. It is not
a blanket compatibility guarantee for `ds.glm`, `ds.glmSLMA`, `ds.table`
or other packages: each downstream server method must be separately
allowlisted and tested with `omop.table`/`dsomop_protected` objects and
with empty local levels.

Shared factor codes also do not harmonize vocabulary semantics. For
plans that use concept-name translation, descendant or mapped expansion,
dictionaries, or OHDSI vocabulary-dependent scores, strict harmonization
requires one identical non-missing reported vocabulary version and
rechecks that identity before execution. A matching version is necessary
but is not proof of identical local mappings: pin the intended standard
concept IDs or compare expansions whenever the study’s governance
requires stronger semantic assurance.

Extraction-size limits are site policy too. The defaults are 1,000
feature specifications (`dsomop.max_feature_specs`), 1,000 pivoted
concepts (`dsomop.max_pivot_concepts`), 5,000 output columns
(`dsomop.max_output_columns`) and 10,000 temporal bins
(`dsomop.max_temporal_bins`). Filter trees default to depth 32, 1,024
nodes and 10,000 values, while plans default to at most 100 outputs. A
controller may lower any of them. Federated planning negotiates the
minimum compatible value for each cap and rejects or narrows a request
that exceeds it; choosing the largest site’s allowance is not portable.

## Federated Analysis Workflow

``` r

# 1. Connect and attach
conns <- DSI::datashield.login(logins = login_data)
ds.omop.connect(resource = "project.omop_cdm", conns = conns, symbol = "omop")

# 2. Compare schemas
comparison <- ds.omop.compare(symbol = "omop", conns = conns)

# 3. Build a plan on common ground
plan <- ds.omop.plan()
plan <- ds.omop.plan.cohort(plan,
  spec = list(type = "condition", concept_set = c(201826)))
plan <- ds.omop.plan.baseline(plan,
  columns = c("gender_concept_id", "race_concept_id"))
plan <- ds.omop.plan.features(plan, name = "labs", table = "measurement",
  specs = list(omop.feature.mean_value(concept_set = c(3004410))))

# 4. Harmonize, preview, execute
plan <- ds.omop.plan.harmonize(plan, mode = "intersection",
  symbol = "omop", conns = conns)
ds.omop.plan.preview(plan, symbol = "omop", conns = conns)
ds.omop.plan.execute(plan, out = c(baseline = "D", labs = "L"),
  symbol = "omop", conns = conns)

# 5. Run only downstream methods that the deployment has reviewed and tested
#    for omop.table inputs and this output grain.

# 6. Clean up
ds.omop.disconnect(symbol = "omop", conns = conns)
DSI::datashield.logout(conns)
```

## CDM and Database Compatibility

dsOMOP ships reviewed metadata for OMOP CDM 5.3 and 5.4 and can use the
optional OHDSI `CommonDataModel` package when its exact version is
supported. An unknown CDM version fails closed. Custom tables or columns
are not exposed merely because introspection finds them: the controller
must add them to `dsomop.allowed_cdm_extensions` and review their joins
and disclosure class.

The server declares adapters for PostgreSQL, SQLite, DuckDB,
MySQL/MariaDB, SQL Server/Synapse/PDW, Oracle, Redshift, BigQuery,
Snowflake and Spark/Databricks. This is an implementation profile, not
proof that every live vendor/driver/version combination has passed
integration testing. SQLite has executable integration coverage and
DuckDB has optional embedded coverage; network engines currently have
SQL-contract tests only. Temporary-object support also differs, so
cross-statement, temp-dependent recipes are unavailable on SQL
Server/Synapse/PDW, Oracle and BigQuery. Test the intended driver,
permissions, catalog layout and plan on every participating site.

Capabilities are introspected on connect and cached client-side.
Reconnect after a schema, CDM metadata, server package or
disclosure-policy change.

## Common Age, Date and Count Policy

For a multi-server session, dsOMOPClient rejects incompatible versions
or semantics rather than silently accepting the first server’s
configuration. It uses the largest minimum age/date range, accepts age
bands only on boundaries common to every public server grid, and
requires matching calendar semantics, day granularity, timezone and week
start. Age is currently annual-resolution
`reference_year - year_of_birth`, not birthday-aware completed age. This
is the same definition used by OHDSI Circe cohort criteria; nullable
OMOP birth month/day fields are not used to create site-dependent
pseudo-precision.

Count pooling additionally requires identical `dsomop.nfilter.band`
widths. Already banded site counts cannot in general be re-binned
exactly, and pooled counts remain sums of lower bounds. A missing or
suppressed period is never silently interpreted as zero.
