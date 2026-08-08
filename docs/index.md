# dsOMOPClient

## Introduction

`dsOMOPClient` is the analyst-facing interface for typed operations over
remote [OMOP Common Data Model
(CDM)](https://www.ohdsi.org/data-standardization/) resources exposed
through
[DataSHIELD](https://www.datashield.org/about/about-datashield-collated).
It builds plans and recipes client-side; the companion `dsOMOP` package
validates and executes them beside the database.

This is not an arbitrary SQL or arbitrary-join gateway. Its usable
surface is the set of reviewed table, filter, cohort, output and
aggregate contracts implemented by the server. Installing the client
does not by itself guarantee disclosure safety: that also depends on the
server method allowlist, effective `nfilter`/`dsomop.*` policy, database
privileges and any downstream package that can consume the assigned
objects.

Key features include:

- **Typed plans and recipes:** selections, concept scopes, nested
  reviewed filters, cohort scopes, visit links, event windows and
  several output grains.
- **Longitudinal outputs:** recurrent cohort episodes, event-long,
  episode-grain wide/features, survival, interval-long, time-binned
  sparse covariates and regular person-period panels linked by a stable
  cohort-row key.
- **Controlled exploration:** schema/vocabulary discovery and aggregate
  profiling under endpoint-specific server disclosure policies.
- **Federated checks:** automatic strict schema/semantic harmonization
  for multi-server plans, common age/date/capacity negotiation and
  deterministic concept-factor coordination. Heterogeneous DBMS
  deployments still need live integration validation.
- **Staged execution:** private server-local Parquet, or CSV fallback,
  validated package-neutral descriptors for bounded workflows that
  should not keep the final table in the DataSHIELD R session.

[`ds.omop.connect()`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.connect.md)
fails closed unless every server returns a complete AggregateMethods
inventory, and rejects methods named `c`/`list` or aliases whose target
is `c`, `list`, [`base::c`](https://rdrr.io/r/base/c.html) or
[`base::list`](https://rdrr.io/r/base/list.html). Those generic
constructors can otherwise wrap and return a protected server object
without a reviewed disclosure gate. This preflight is defence in depth:
the controller must remove the methods from the complete global
DataSHIELD profile, because a caller using DSI directly can bypass the
client package.

## Structure

The ecosystem has two components:

- **Server-side `dsOMOP`:** owns database connections, schema policy,
  pseudonymisation, extraction and disclosure gates. Its README contains
  the actual DBMS matrix, deployment boundary and OHDSI integration
  status:
  [isglobal-brge/dsOMOP](https://github.com/isglobal-brge/dsOMOP).

- **Client-side `dsOMOPClient`:** constructs and serializes typed
  requests, negotiates selected federated contracts, and coordinates
  returned server objects. It never receives database credentials or raw
  person identifiers.

## Installation

To install the client-side package `dsOMOPClient`, follow the steps
below. This guide assumes you have R installed on your system and the
necessary permissions to install R packages.

The `dsOMOPClient` package can be installed directly from GitHub using
the `devtools` package. If you do not have `devtools` installed, you can
install it using the following command in R:

``` r

install.packages("devtools")
```

You can then install the `dsOMOPClient` package using the following
command in R:

``` r

devtools::install_github('isglobal-brge/dsOMOPClient')
```

Once the package is installed, you can load it into your R environment
using the following command:

``` r

library(dsOMOPClient)
```

## Dedicated sticky privacy releases

When the custodian has enabled the dedicated service, inspect its
contract and request a typed person-bounded statistic from an eligible
server-side plan or reviewed loader output:

``` r

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

The client cannot choose epsilon, a seed, nonce, epoch or reroll.
Domains, date breaks, clipping bounds and longitudinal contribution caps
are public parts of the request; noise allocation, sticky identity and
the durable ledger remain server-owned. See
[`?omop_privacy`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_privacy.md)
and
[`?ds.omop.dp.release`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.dp.release.md)
for the seven supported primitives and their reducers. Before any
release, the client checks the server continuity identifiers and refuses
a federated request through two connections that share a logical noise
domain, a ledger, or the same domain-scoped ledger authentication key.
These checks prevent duplicate-node pooling while replicas converge
during a noise-root rotation and detect accidentally forked durable
ledgers. Such a rotation changes `noise_key_id`, `noise_domain_id` and
`privacy_instance_id`, while `ledger_id` and `ledger_key_id` remain
stable; operators should refresh any monitoring pins after the
coordinated rollout.

## Current boundaries

Plans and recipes cover common epidemiological extraction shapes, but
not every possible relational or longitudinal estimand. In particular:

- multi-table `long` recipes split into one output per source table;
  there is no arbitrary cross-table joined-long output;
- federated `wide` output requires a closed integer `concept_set` and
  `translate_concepts = FALSE`; every declared concept has the same
  concept-ID-derived column on every node (filled with `NA` when locally
  absent), and no undeclared concept can enter the output. Wide output
  also requires at most one event per declared grain and concept, so the
  request must use deterministic event selection or an explicit
  reduction;
- `event_select` defaults to global selection within a person/episode
  and can use `by = "concept"` for independent first/last-N selection
  per concept;
- recurrent cohort episodes, regular episode-by-period panels, and named
  competing-risk, recurrent-event and counting-process outputs are
  first-class contracts; arbitrary multi-state transition models remain
  outside the reviewed output contracts;
- sparse output supports person or indexed episode grain and includes a
  complete `personRef`; absent covariate rows represent zero for roster
  members with no qualifying event;
- the local Query Library is curated and incomplete. The opt-in privacy
  path currently supports seven person-bounded sticky-noise primitives
  with a durable authenticated ledger. Its single public guarantee is
  `sticky_person_bounded_noise_with_authenticated_lineage_and_nominal_accounting`;
  there is no separate certification mode. Eligible inputs carry
  authenticated semantic lineage and deterministic person-level
  contribution bounds. The pinned upstream snapshot is exhaustively
  classified as 129 executable bounded redesigns, 54
  vocabulary/reference metadata questions and 18 blocked shapes; none
  authorizes literal upstream SQL.

Servers also impose configurable operational shape caps (by default
1,000 feature specifications, 1,000 pivoted concepts, 5,000 output
columns and 10,000 temporal bins, 100 selected events per episode/source
group, plus filter trees of depth 32, 1,024 nodes and 10,000 values, and
100 outputs per plan). Federated planning must respect the minimum
compatible value across participating servers. These bounds limit
memory/CPU amplification; they are separate from disclosure thresholds.

Staged descriptors point to private server-local files. Successful local
staging writes a version-2 manifest only after every file and descriptor
is complete. Each component carries an exact semantic contract, while
components of one composite output share a bundle contract and
pseudonym-key identity. Consumers must use the server-side resolver to
validate those contracts and the path rather than opening an embedded
filename directly. Descriptors are not downloads and do not grant access
to another service identity; cross-service consumption requires a
separately reviewed broker. SQL-backed long-event, intervals, survival,
temporal-covariate and person-period components stream without
materialising the complete result in R; wide/features, baseline and
person-level outputs still materialise before staging. Execution is
all-or-none for DataSHIELD-visible symbols, not a distributed filesystem
transaction: after a cross-node failure, already committed private files
may remain until handle cleanup, disconnect or TTL cleanup. See the
*Data Extraction*, *Multi-Server* and *Security* vignettes for the
precise contracts and limits.

## Community development and extensions

Extensions that consume `omop.table` objects or staged descriptors
become part of the disclosure boundary. They should be separately
reviewed and allowlisted; the class name alone does not make a
downstream method safe.

An example is
**[`dsOMOPHelper`](https://github.com/isglobal-brge/dsOMOPHelper)**,
which combines calls from `dsOMOPClient` and `dsBaseClient` for common
workflows. It is a separate package and must be reviewed against the
same server allowlist and disclosure policy; this README does not assert
compatibility with every dsOMOP output or deployment.

## Acknowledgements

- The development of dsOMOP has been supported by the
  **[RadGen4COPD](https://github.com/isglobal-brge/RadGen4COPD)**,
  **[P4COPD](https://www.clinicbarcelona.org/en/projects-and-clinical-assays/detail/p4copd-prediction-prevention-personalized-and-precision-management-of-copd-in-young-adults)**,
  **[CADSET](https://www.ersnet.org/science-and-research/clinical-research-collaboration-application-programme/cadset-chronic-airway-diseases-early-stratification/)**,
  and **[DATOS-CAT](https://datos-cat.github.io/LandingPage)** projects.
  These collaborations have not only provided essential financial
  backing but have also affirmed the project’s relevance and application
  in significant research endeavors.
- This project has received funding from the **[Spanish Ministry of
  Education, Innovation and
  Universities](https://www.ciencia.gob.es/en/)**, the **[National
  Agency for Research](https://www.aei.gob.es/en)**, and the **[Fund for
  Regional
  Development](https://ec.europa.eu/regional_policy/funding/erdf_en)**
  **(PID2021-122855OB-I00)**. We also acknowledge support from the grant
  **CEX2023-0001290-S** funded by **MCIN/AEI/10.13039/501100011033**,
  and support from the **[Generalitat de
  Catalunya](https://web.gencat.cat/en/inici/index.html)** through the
  **[CERCA Program](https://cerca.cat/en/)** and the **Consolidated
  Group on HEALTH ANALYTICS (2021 SGR 01563)**.
- Additionally, this project has received funding from the **[Instituto
  de Salud Carlos III (ISCIII)](https://www.isciii.es/)** through the
  project **“PMP21/00090,”** co-funded by the **[European
  Union’s](https://european-union.europa.eu/index_en)** **Resilience and
  Recovery Facility**. It has also been partially funded by the
  **“Complementary Plan for Biotechnology Applied to Health,”**
  coordinated by the **[Institut de Bioenginyeria de Catalunya
  (IBEC)](https://ibecbarcelona.eu/)** within the framework of the
  **Recovery, Transformation, and Resilience Plan (C17.I1)** – Funded by
  the **[European Union](https://european-union.europa.eu/index_en)** –
  **[NextGenerationEU](https://next-generation-eu.europa.eu/index_en)**.

## Contact

For further information or inquiries, please contact:

- **Juan R González**: <juanr.gonzalez@isglobal.org>
- **David Sarrat González**: <david.sarrat@isglobal.org>

For more details about **DataSHIELD**, visit
<https://www.datashield.org>.

For more information about the **Barcelona Institute for Global Health
(ISGlobal)**, visit <https://www.isglobal.org>.
