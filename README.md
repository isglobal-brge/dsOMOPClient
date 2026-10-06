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

Since dsOMOP 2.7.0, an enabled DP layer defaults to the exclusive channel:
standard population-statistics helpers refuse access and point to
`ds.omop.dp.release()`. Only the custodian can restore standard statistics with
`dsomop.dp.exclusive = FALSE`; no client option bypasses the policy or noise.
`ds.omop.dp.status()` prints each server's exclusivity. Client 2.7.3 automatically
prepares memory plans and recipes without observed factor-level discovery when
any selected server is exclusive, with a message explaining that concept IDs
or translated names remain unchanged. For category counts, use the typed
categorical histogram with a public level domain. Servers at 2.6.0 that omit
`exclusive` retain their existing behavior.

The dedicated release service is enabled by default on dsOMOP servers since
2.6.0; custodians can opt out with `dsomop.dp.enabled = FALSE` or
`DSOMOP_DP_ENABLED=0`. Initialize the OMOP resource before inspecting its DP
contract: unconfigured servers derive domain and snapshot identifiers from the
resource and CDM source metadata and require persistent private state storage.
Custodians advance the public snapshot or `dsomop.dp.privacy_epoch` and restart
sessions for each planned publication, including no-op refreshes. Earlier server
versions require explicit enablement. Inspect the contract and request a typed
person-bounded statistic from an eligible server-side plan or reviewed loader
output:

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
  "measurement_events", privacy, datasources = conns, format = "long"
)
```

The client cannot choose epsilon, a seed, nonce, epoch or reroll. Domains,
date breaks, clipping bounds and longitudinal contribution caps are public
parts of the request; fixed per-release epsilon and sticky identity remain
server-owned. Since server 2.7.1, one permanent first answer is stored per public
request, snapshot and privacy epoch. Every later valid request returns the same
complete response, even after one or arbitrarily many persons change. Answers
remain stale until the custodian rotates the snapshot or epoch and restarts
sessions. Different requests first answered at different times can reflect
different source versions; they are not a coherent database snapshot.

Client 2.7.4 defaults to requiring the v3
`fixed_per_release_snapshot_first_answer_v1` contract for releases. Status reports
`history_dependent = TRUE`, `persistent_state = "noise_root_and_release_bindings"`,
`release_binding = "snapshot_first_answer_v1"`,
`privacy_call_quota = "none"`, and
`service_capacity = "public_identity_reservations_v1"`. Older v2 status remains
inspectable and is printed as legacy. Releases from legacy sites are refused
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

The server derives a first answer using the existing persistent root, private
bounded-statistic fingerprint and calibrated mechanism. Later successful replies
add no payload observation for the same identity and public admission schedule.
This is no full temporal transcript DP guarantee: distinct requests and epochs,
private-triggered rotation, validation failures and timing remain separate.
There is no lifetime privacy budget or call counter. See `?omop_privacy` and
`?ds.omop.dp.release` for the seven supported primitives and their reducers. Before any release, the client
refuses a federated request through two connections that report either the same
`noise_domain_id` or the same server-owned logical `domain`. This prevents one
logical privacy node from being pooled twice, including through connections
that expose different noise material.

Fresh state and upgrades from 2.7.0 initialize the release store automatically
on first DP use, recording its UUID in the owner-only `release-store-id` pin
file in the state root, outside `release-bindings/`. Explicit setup with
`omopInitializeReleaseStore()` remains available. Custodians should additionally
pin the UUID outside `DSOMOP_STATE_DIR`; see the paired
[dsOMOP README](https://github.com/isglobal-brge/dsOMOP#sticky-noise) for
configuration. An externally configured UUID takes precedence and must match
the local pin and store header. A retained local or external pin detects a
missing store. Without an external UUID, deleting the whole state root is
equivalent to a fresh install: the normal file-backed setup creates a new noise
root and release domain; an injected root remains externally controlled.
Deleting both the store and its local pin while retaining the root is a
residual that only external pinning prevents.
Retain the root and authenticated bindings together, including with injected
roots and across replicas. Back up and restore the complete consistent state
with workers stopped; whole-state rollback prevention is an operational
assumption. Lost or corrupt established state fails closed and cannot be
repaired by deleting the store or replacing the noise root. Storage reserves a
public maximum per new request, defaults to 1 GiB via
`dsomop.dp.release_store_bytes`, and never evicts bindings. Capacity can be
increased without changing answers; previously admitted requests remain
readable when new identities are refused. These credits limit service storage,
not epsilon expenditure.

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
