# dsOMOPClient

## Introduction

`dsOMOPClient` is the analyst-facing interface for typed operations over remote
[OMOP Common Data Model (CDM)](https://www.ohdsi.org/data-standardization/)
resources exposed through
[DataSHIELD](https://www.datashield.org/about/about-datashield-collated). It
builds plans and recipes client-side; the companion `dsOMOP` package validates
and executes them beside the database.

This is not an arbitrary SQL or arbitrary-join gateway. Its usable surface is
the set of reviewed table, filter, cohort, output and aggregate contracts
implemented by the server. Installing the client does not by itself guarantee
disclosure safety: that also depends on the server method allowlist, effective
`nfilter`/`dsomop.*` policy, database privileges and any downstream package that
can consume the assigned objects.

Key features include:

- **Typed plans and recipes:** selections, concept scopes, nested reviewed
  filters, cohort scopes, visit links, event windows and several output grains.
- **Longitudinal outputs:** recurrent cohort episodes, event-long,
  episode-grain wide/features, survival, interval-long, time-binned sparse
  covariates and regular person-period panels linked by a stable cohort-row key.
- **Controlled exploration:** schema/vocabulary discovery and aggregate
  profiling under endpoint-specific server disclosure policies.
- **Federated checks:** automatic strict schema/semantic harmonization for
  multi-server plans, common age/date/capacity negotiation and deterministic
  concept-factor coordination. Heterogeneous DBMS deployments still need live
  integration validation.
- **Staged execution:** private server-local Parquet, or CSV fallback,
  validated package-neutral descriptors for bounded workflows that should not
  keep the final table in the DataSHIELD R session.

`ds.omop.connect()` fails closed unless every server returns a complete
AggregateMethods inventory, and rejects methods named `c`/`list` or aliases
whose target is `c`, `list`, `base::c` or `base::list`. Those generic
constructors can otherwise wrap and return a protected server object without a
reviewed disclosure gate. This preflight is defence in depth: the controller
must remove the methods from the complete global DataSHIELD profile, because a
caller using DSI directly can bypass the client package.

## Structure

The ecosystem has two components:

- **Server-side `dsOMOP`:** owns database connections, schema policy,
  pseudonymisation, extraction and disclosure gates. Its README contains the
  actual DBMS matrix, deployment boundary and OHDSI integration status:
  [isglobal-brge/dsOMOP](https://github.com/isglobal-brge/dsOMOP).

- **Client-side `dsOMOPClient`:** constructs and serializes typed requests,
  negotiates selected federated contracts, and coordinates returned server
  objects. It never receives database credentials or raw person identifiers.

## Installation

To install the client-side package `dsOMOPClient`, follow the steps below. This guide assumes you have R installed on your system and the necessary permissions to install R packages.

The `dsOMOPClient` package can be installed directly from GitHub using the `devtools` package. If you do not have `devtools` installed, you can install it using the following command in R:
```R
install.packages("devtools")
```

You can then install the `dsOMOPClient` package using the following command in R:
```R
devtools::install_github('isglobal-brge/dsOMOPClient')
```

Once the package is installed, you can load it into your R environment using the following command:
```R
library(dsOMOPClient)
```

### Checking versions

After `DSI::datashield.login()`, and before connecting to an OMOP resource,
you can check what each server offers:
```R
ds.omop.client.version()          # this client
ds.omop.server.version(conns)     # dsOMOP on every server
```
`ds.omop.server.version()` reports, per server, the dsOMOP version whose
DataSHIELD methods the profile publishes, the version installed (the one that
answers), the methods this client calls that the profile lacks, and what to
change when a server is not ready. On Opal, upgrading dsOMOP does not publish
the new version's methods: an administrator publishes its settings again
(Administration > DataSHIELD > Packages > dsOMOP > Publish).
`ds.omop.connect()` stops with the same explanation when a server cannot be
used, and reports each server's own error instead of DSI's generic notice.

## Recipe language 2 in 2.10.4

Use `omop_study()` for a study with explicit baseline and follow-up inputs, or
compose `omop_event()`, `omop_population()`, `omop_reduce()` and `omop_output()`
for the advanced layer. Both produce the same typed recipe language;
`print()` shows defaults and `recipe_to_code()` prints the advanced expansion.
See [populations and studies](vignettes/v03_cohorts.Rmd) and
[events and outputs](vignettes/v04_extraction.Rmd), including coded education
level and cause of death. Examples remain `eval = FALSE` until the integrated
compilers pass acceptance; advertised operations determine what a build runs.

| Backend | Required 2.10.4 verification | Runtime release target |
|---|---|---|
| PostgreSQL | Live typed SQL and recipe tests | 16 (development pod: 14) |
| SQLite / RSQLite | Embedded tests and runtime window-frame probe | Actual SQLite and RSQLite versions recorded |
| DuckDB | Optional embedded tests | Actual engine/driver version recorded |
| MySQL / MariaDB | Live vendor tests; floors 8.0 / 10.2 | 8.4 / 11.4 (pod: 8.4.11 / 10.6) |
| SQL Server / Synapse / PDW, Oracle, Redshift, BigQuery, Snowflake, Spark / Databricks | Rendered SQL contracts only; no cross-statement temporary materialisation | No live runtime claim |
| Opal / Rock | O1–O12 on both pinned Opal versions | Opal 6.1.3 and 5.7.5; R ≥4.2 |

The server README contains the construct-by-construct matrix. A supported
connector alone does not establish recipe support. Unsupported constructs
refuse at preflight with `dialect_unsupported`. Check both installed and
published package versions with `ds.omop.server.version()`; use matching
2.10.4 packages and republish methods and Options on Opal after upgrading.

Small private inputs are allowed. Each independently accessible final patient
component needs `nfilter.subset` distinct persons. Strict servers refuse the
whole plan on a small root; non-strict servers with `on_small_site = "error"`
return passing families and warn with `partial` status. With `"skip"`, either
server mode commits nothing at a small site and returns NULL for it; passing
sites remain. Only support failure is skippable. Technical failures roll back
the federation. Keep the returned dynamic inventory rather than guessing names.

## Federated result views

Aggregate APIs use the same result vocabulary as dsBaseClient:

```R
per_site <- ds.omop.analysis.run("dsomop:incidence.rate", type = "split")
pooled   <- ds.omop.analysis.run("dsomop:incidence.rate", type = "combine")
both     <- ds.omop.analysis.run("dsomop:incidence.rate", type = "both")
```

The unified catalog covers QueryLibrary redesigns, Achilles, CohortDiagnostics,
CohortIncidence, Characterization, CohortMethod, SCCS, PLP,
EvidenceSynthesis, TreatmentPatterns and FeatureExtraction-style local ports.
Every aggregate catalog entry publishes a server-owned pooling strategy:
additive sufficient statistics, reconstructed ratios, weighted moments and
pooled variance, inverse-variance effects, Kaplan-Meier risk sets, or an
explicit `not_poolable` reason. The client never guesses the algebra from a
column name. `ds.omop.ohdsi.results()` applies the same rule to reviewed
physical OHDSI result tables and never substitutes a same-named live analysis.

Pooled counts are sums of site contributions. Without privacy-preserving record
linkage, a person represented in two databases contributes once in each; local
distinct-person counts likewise cannot be turned into a global set union.
Study/cohort identifiers and public bin definitions must describe the same
estimand on every participating node.

## Dedicated sticky privacy releases

Differential privacy (DP) concerns only the OHDSI community tools integrated in
dsOMOP (Achilles results, OHDSI results tables, the analysis catalog and the
query library). Connecting, loading tables, filters, concept factors, merges,
cohorts and dsOMOP's own exploration never use it: since server 2.8.0 they run
under the standard DataSHIELD disclosure controls only, as before DP was added.
The community tools are enabled by default; a custodian can disable them with
`dsomop.tools.enabled = FALSE`. Since server 2.9.0 their protection is
automatic (`dsomop.tools.mode = "auto"`, nothing to configure): outputs that
the standard DataSHIELD controls protect adequately stay under those controls,
and the query-library questions about per-person durations, per-person counts
and demographic distributions are answered as histograms of one value per
person (or complete public partitions) with sticky DP noise, or under the
standard controls when a server's DP service is unavailable.
`ds.omop.analysis.run()` reports each server's protection in
`meta$protection` and prints it. Other questions use the typed sticky
channel: `ds.omop.dp.prepare()` prepares a DP-sealed person-local table from a
recipe or plan (the analyst picks the population and variables, such as an
age at a public reference year), and `ds.omop.querylibrary.sticky.release()`
does that preparation itself when the redesign carries its recipe. A custodian can choose `"standard"` (no DP)
or `"dp"` (DP only; `dsomop.dp.exclusive = TRUE` selects it), in which case
the tools refuse every standard statistic; no client option bypasses the
policy or noise. `ds.omop.dp.status()` prints each server's exclusivity and
tools mode. (Servers 2.7.x applied exclusivity to standard statistics and
refused factor discovery; with them, set `factor_concepts = FALSE` or upgrade
the server.)

The dedicated release service is enabled by default on dsOMOP servers since
2.6.0; custodians can opt out with `dsomop.dp.enabled = FALSE` or
`DSOMOP_DP_ENABLED=0`. Unconfigured servers derive domain and snapshot
identifiers from the resource and CDM source metadata and require persistent
private state storage. Custodians advance the public snapshot or
`dsomop.dp.privacy_epoch` and restart sessions for each planned publication,
including no-op refreshes. Inspect the contract and request a typed
person-bounded statistic from an output of a community tool (for example an
analysis-catalog loader); tables loaded through plans or recipes carry no DP
provenance:

```R
ds.omop.dp.status(conns)

privacy <- omop_privacy(
  "numeric_histogram",
  variable = "measurement_date",
  breaks = c("2025-01-01", "2025-04-01", "2025-07-01", "2026-01-01"),
  reducer = "records",
  max_contributions = 2L,
  order_by = "measurement_date"
)
result <- ds.omop.dp.release(
  "tool_output", privacy, datasources = conns, format = "long"
)
```

The client cannot choose epsilon, a seed, nonce, epoch or reroll. Domains,
date breaks, clipping bounds and longitudinal contribution caps are public
parts of the request; fixed per-release epsilon and sticky identity remain
server-owned. Since server 2.10.0, every answer is a deterministic function of
the server's secret noise root, the question, the protected statistic and a
private automatic database version: the same question on the same data returns
the same complete response, also after restarts, and nothing but the root is
stored. A database reload that adds rows re-draws every answer at once. Servers
2.7.1 to 2.9.x instead stored one permanent first answer per public request,
snapshot and privacy epoch, stale until the custodian rotated the snapshot or
epoch.

Client 2.10.0 accepts both v3 contracts for releases, alone or mixed in one
federation: the seed-only `fixed_per_release_seed_data_versioned_v1` (status
`history_dependent = FALSE`, `persistent_state = "noise_root_only"`,
`release_binding = "seed_data_versioned_v1"`, `service_capacity = "none"`) and
the first-answer `fixed_per_release_snapshot_first_answer_v1`
(`history_dependent = TRUE`,
`persistent_state = "noise_root_and_release_bindings"`,
`release_binding = "snapshot_first_answer_v1"`,
`service_capacity = "public_identity_reservations_v1"`). Both report
`privacy_call_quota = "none"`. Older v2 status remains inspectable and is
printed as legacy. Releases from legacy sites are refused
with an upgrade message unless the analyst explicitly sets:

```r
options(dsomop.dp.allow_legacy_servers = TRUE) # default FALSE
```

This transition option supports federations upgrading site by site, including
mixed v2/v3 releases. Each legacy server produces a warning: **no first-answer
binding: an unrotated data refresh can reveal whether a released statistic
changed; see isglobal-brge/dsOMOP#20**. Shared mechanism, provenance, public
harmonization and payload checks still apply. Every result records each site's
protocol and contract in `result$meta$privacy$per_site_contract`, plus
`legacy_servers` and `mixed_contracts`, even with `type = "combine"`. Contract
fields that differ across sites are `NULL` in the shared privacy metadata;
consult the per-site map. The warnings are also saved in `result$meta$warnings`.
Return the option to `FALSE` once every site has upgraded.

The server derives each answer from its persistent root, the private
bounded-statistic fingerprint and the calibrated mechanism, so repeating a
question adds no observation. This is no full temporal transcript DP guarantee:
distinct requests, database versions and epochs, validation failures and timing
remain separate.
There is no lifetime privacy budget or call counter. See `?omop_privacy` and
`?ds.omop.dp.release` for the seven supported primitives and their reducers. Before any release, the client
refuses a federated request through two connections that report either the same
`noise_domain_id` or the same server-owned logical `domain`. This prevents one
logical privacy node from being pooled twice, including through connections
that expose different noise material.

Server-side DP state belongs to the custodian. Since dsOMOP 2.10.0 it is only
the noise root, created on first use or injected by the custodian; see the
paired [dsOMOP README](https://github.com/isglobal-brge/dsOMOP#sticky-noise). A
damaged root makes the server refuse DP releases with one generic error;
dsOMOP's core functions never use DP.

## Current boundaries

Plans and recipes cover common epidemiological extraction shapes, but not every
possible relational or longitudinal estimand. In particular:

- multi-table `long` recipes split into one output per source table; there is no
  arbitrary cross-table joined-long output;
- federated `wide` output requires a closed integer `concept_set` and
  `translate_concepts = FALSE`; every declared concept has the same
  concept-ID-derived column on every node (filled with `NA` when locally
  absent), and no undeclared concept can enter the output. Wide output also
  requires at most one event per
  declared grain and concept, so the request must use deterministic event
  selection or an explicit reduction;
- `event_select` defaults to global selection within a person/episode and can
  use `by = "concept"` for independent first/last-N selection per concept;
- recurrent cohort episodes, regular episode-by-period panels, and named
  competing-risk, recurrent-event, counting-process and graph-declared
  multi-state outputs are first-class contracts. Multi-state plans accept an
  `mstate` transition matrix or an equivalent public adjacency graph, including
  cycles and repeated visits to a state; the graph cannot be inferred privately
  from site data and arbitrary SQL remains outside the contract;
- sparse output supports person or indexed episode grain and includes a complete
  `personRef`; absent covariate rows represent zero for roster members with no
  qualifying event;
- the local Query Library is curated and incomplete. The dedicated privacy path
  currently supports seven person-bounded sticky-noise primitives. Its public
  guarantee is `sticky_person_bounded_discrete_laplace_per_release_v1`, under
  the `fixed_per_release_snapshot_first_answer_v1` contract. Eligible inputs carry
  authenticated semantic lineage and deterministic person-level contribution
  bounds. The
  pinned upstream snapshot is exhaustively classified as 129 executable bounded
  redesigns, 54 vocabulary/reference metadata questions and 18 blocked shapes;
  none authorizes literal upstream SQL.

Servers also impose configurable operational shape caps (by default 1,000
feature specifications, 1,000 pivoted concepts, 5,000 output columns and 10,000
temporal bins, 100 selected events per episode/source group, plus filter trees
of depth 32, 1,024 nodes and 10,000 values, and 100 outputs per plan). Federated
planning must respect the minimum compatible value across participating
servers. These bounds limit memory/CPU
amplification; they are separate from disclosure thresholds.

Staged descriptors point to private server-local files. Successful local
staging writes a version-2 manifest only after every file and descriptor is
complete. Each component carries an exact semantic contract, while components
of one composite output share a bundle contract and pseudonym-key identity.
Consumers must use the server-side resolver to validate those contracts and the
path rather than opening an embedded filename directly. Descriptors are not
downloads and do not grant access to another service identity; cross-service
consumption requires a separately reviewed broker. SQL-backed
long-event, intervals, survival, temporal-covariate and person-period components
stream without materialising the complete result in R; wide/features, baseline
and person-level outputs still materialise before staging.
Execution is all-or-none for DataSHIELD-visible symbols, not a distributed
filesystem transaction: after a cross-node failure, already committed private
files may remain until handle cleanup, disconnect or TTL cleanup. See the *Data
Extraction*, *Multi-Server* and *Security* vignettes for the precise contracts
and limits.

## Community development and extensions

Extensions that consume `omop.table` objects or staged descriptors become part
of the disclosure boundary. They should be separately reviewed and allowlisted;
the class name alone does not make a downstream method safe.

An example is **[`dsOMOPHelper`](https://github.com/isglobal-brge/dsOMOPHelper)**,
which combines calls from `dsOMOPClient` and `dsBaseClient` for common workflows.
It is a separate package and must be reviewed against the same server allowlist
and disclosure policy; this README does not assert compatibility with every
dsOMOP output or deployment.

## Acknowledgements

- The development of dsOMOP has been supported by the **[RadGen4COPD](https://github.com/isglobal-brge/RadGen4COPD)**, **[P4COPD](https://www.clinicbarcelona.org/en/projects-and-clinical-assays/detail/p4copd-prediction-prevention-personalized-and-precision-management-of-copd-in-young-adults)**, **[CADSET](https://www.ersnet.org/science-and-research/clinical-research-collaboration-application-programme/cadset-chronic-airway-diseases-early-stratification/)**, and **[DATOS-CAT](https://datos-cat.github.io/LandingPage)** projects. These collaborations have not only provided essential financial backing but have also affirmed the project's relevance and application in significant research endeavors.
- This project has received funding from the **[Spanish Ministry of Education, Innovation and Universities](https://www.ciencia.gob.es/en/)**, the **[National Agency for Research](https://www.aei.gob.es/en)**, and the **[Fund for Regional Development](https://ec.europa.eu/regional_policy/funding/erdf_en)** **(PID2021-122855OB-I00)**. We also acknowledge support from the grant **CEX2023-0001290-S** funded by **MCIN/AEI/10.13039/501100011033**, and support from the **[Generalitat de Catalunya](https://web.gencat.cat/en/inici/index.html)** through the **[CERCA Program](https://cerca.cat/en/)** and the **Consolidated Group on HEALTH ANALYTICS (2021 SGR 01563)**.
- Additionally, this project has received funding from the **[Instituto de Salud Carlos III (ISCIII)](https://www.isciii.es/)** through the project **"PMP21/00090,"** co-funded by the **[European Union's](https://european-union.europa.eu/index_en)** **Resilience and Recovery Facility**. It has also been partially funded by the **"Complementary Plan for Biotechnology Applied to Health,"** coordinated by the **[Institut de Bioenginyeria de Catalunya (IBEC)](https://ibecbarcelona.eu/)** within the framework of the **Recovery, Transformation, and Resilience Plan (C17.I1)** – Funded by the **[European Union](https://european-union.europa.eu/index_en)** – **[NextGenerationEU](https://next-generation-eu.europa.eu/index_en)**.

## Contact

For further information or inquiries, please contact:

- **Juan R González**: juanr.gonzalez@isglobal.org
- **David Sarrat González**: david.sarrat@isglobal.org

For more details about **DataSHIELD**, visit [https://www.datashield.org](https://www.datashield.org).

For more information about the **Barcelona Institute for Global Health (ISGlobal)**, visit [https://www.isglobal.org](https://www.isglobal.org).
