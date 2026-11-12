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

Protected releases concern the OHDSI community tools integrated in dsOMOP
(Achilles, OHDSI results tables, the analysis catalogue and QueryLibrary).
Connecting, loading, filtering, factors, merges, cohorts and core exploration
continue under their standard DataSHIELD controls. Community tools default to
`dsomop.tools.mode = "auto"`; the client cannot bypass the server's policy.

On dsOMOP 2.10.3 there are two protected routes. `fixed_synopsis` uses a declared
bounded vector for one automatic dataset generation. Its finite epsilon and
delta zero apply before the exact support gates. `realised_sticky` reuses
secret fingerprints of bounded contributions; it is heuristic and claims no
mechanism epsilon or delta. Neither route claims privacy for the whole API
transcript. `meta$protection` and `meta$privacy` identify the applicable route.
DP-only mode (`dsomop.tools.mode = "dp"` or `dsomop.dp.exclusive = TRUE`) admits
only supported fixed-synopsis routes. Missing protected components never fall
back to exact versions of those components.

The service is enabled by default and needs no custodian configuration for
correct operation. The server owns its persistent noise root and pseudonym key.
For fixed-synopsis requests it recomputes the person-membership generation
inside the request's database snapshot; no per-session generation cache is
used. Updates to clinical values without a membership change are not a new
generation: contribution fingerprints still track changed components, and
cross-version inference remains a stated residual. The public `snapshot_id`
is optional display metadata with no effect on admission or noise. A privacy
epoch is optional hardening, not an update workflow or transcript guarantee.

Use `ds.omop.dp.prepare()` to produce an authenticated person-local table from
a reviewed plan/recipe, or use a supported community-tool loader. Then inspect
the service and request a bounded statistic:

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

The client cannot choose epsilon, a seed, nonce, epoch or reroll. Categorical
requests select views of complete registered public domains; positive binary
masks and nonmissing denominators select cells of the same protected basis.
Numeric/date bins coarsen a common atom basis. Pooling sums compatible signed
components once and applies the same complete-basis projection as the server.
Weighted record histograms reconcile to a protected record total, never a
person total. Unregistered record scales expose coarse histograms and interval
quantiles with explicit tail-censoring flags instead of invented bounded means.
Recurrent event-time `persons` counts distinct contributors among the same
K-capped occurrences using a complete interval-presence vector (sensitivity 3K).
Its static fractions are N/R .2 each, A/B/C .1 each and P .3. Pooling sums signed
presence coordinates and applies the server's radial feasibility projection;
coarse persons are never sums of finer person counts. Retreat toward the
projection's event-mass anchor can add bias.

Every server older than 2.10.3 receives a plain upgrade warning. Installing
this client does not repair an old server's inference defects. Legacy per-site
results retain their server's contract; mixed old/new protected pooling is
refused. Use `type = "split"` while upgrading sites. A new server clearly
refuses typed status/release preflight from clients 2.10.2 and earlier. The
legacy v2 opt-in `options(dsomop.dp.allow_legacy_servers = TRUE)` remains
available for inspecting old deployments; it never enables protected/legacy
pooling or supplies the missing server fixes.

There is no lifetime privacy budget, query quota, privacy accountant or
history-based admission. Equivalent supported views reuse protected components;
distinct overlapping scopes and changes in data/mechanism can still support
inference. Exact support gates and other enabled DataSHIELD methods remain
outside any fixed-vector guarantee. Protected pooling also requires distinct
logical source identifiers: two connections to one logical node are refused,
and coincidentally equal answers do not establish source independence.

See `?omop_privacy` and `?ds.omop.dp.release` for the primitives and reducers,
and the paired [disclosure-control contract](https://github.com/isglobal-brge/dsOMOP/blob/main/DISCLOSURE_CONTROL.md)
for guarantees, residuals and utility costs.

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
  supports seven person-bounded sticky-noise primitives under the current
  `protected_release_fixed_basis_or_realised_v1` contract. Guarantees are
  route-qualified: a calibrated fixed vector on unchanged source data before
  exact-support admission, or realised-sticky protection without a finite joint
  epsilon claim. Eligible inputs carry authenticated semantic lineage and
  deterministic person-level contribution bounds. The
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
