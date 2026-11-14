# Recipe library implementation report

Date: 2026-11-14. Branch: `feat/2.10.5-library`.

Implemented in `/workspace/dsomop/wt/library/dsOMOPClient` only. The sibling
server remains clean at `18b096ffa72feb27dd7f99fcc2cf1ab43a10dae9`.
No server change, CLI coordinator invocation, push, PR, tag, or full suite was
performed. Testing follows COMMON.md's later, explicit targeted-test rule.

## Commits

- `1d19437`: 24 versioned constructor definitions, local discovery/description/
  instantiation, persistent provenance/constraints, initial plan/digest fixtures.
- `3ca1252`: integrated API corrections, deferred panel merge fix, literal
  controls, complete synthetic result/schema fixtures, DSLite tests, guide and
  entry documentation.
- Final evidence/report commit: recorded by the repository history following
  the tested implementation commit(s).

## Delivered behaviour

`recipe_library_list()`, `recipe_library_describe()` and
`recipe_library_instantiate()` cover all 24 designed entries at template
version 1.0.0. Discovery is local, literal and deterministic. The trusted R
constructors produce ordinary recipes; no template evaluator accepts analyst
text. Required bindings remain unbound, and ordinary plan emission, capability
negotiation, publication and pseudonymisation remain authoritative.

Every ID has installed definition/metadata/documentation and a native output
schema artifact. Every default fixture contains bindings, generated expansion,
a literal-constructor control transcribed from the design, wire plan,
normalised plan, canonical semantic payload, SHA-256, independent synthetic
expected rows, complete native column contracts and an execution manifest.
`panel.measurements` explicitly reports its vocabulary-dependent column schema.
The schema artifacts retain native types/nullability and explain protected
identity representation; concept presentation remains the native labelled
factor contract, not factor codes masquerading as concepts.

## Findings, fixes and tests

| Finding | Fix | Targeted evidence |
|---|---|---|
| No assignment-template discovery or catalogue | Added the three client APIs and 24 trusted definitions under `inst/recipe-library` | `test-recipe-library.R`: all IDs, literal filters, pins, malformed/duplicate catalogue definitions, descriptions, checksums |
| Constraints checked only at instantiation would be lost in unbound workflows | Added metadata-backed binding/default checks and deferred numeric-source validation; preserve origin metadata through merge and JSON/YAML | `test-recipe-library.R`: initial/later/imported invalid bindings, deferred references, defaults, serialization and shared parameter conflicts |
| Event `value` overrides in design T02/T03/T05 reject concept-valued columns in integrated 2.10.4 | Select `value_as_concept_id` through education/employment reducers and death `concept_id` through the first-value reducer | All three per-entry plan/digest controls and four-backend DSLite values; last-missing education, concept-0 employment and recorded-death output |
| A deferred measurement event was prefixed, but the carry-forward key retained its old name | Prefix source policy keys when their source declaration is deferred | All-entry bind-before/after-prefix plan/digest equivalence plus existing merge/deferred-base tests |
| Design example supplied `out=list(...)`, but the existing API accepts a named character vector | Use `out=c(...)` in executable examples and tests; no API extension | Public `recipe_execute()` calls in execution, demo and boundary tests |
| Ordinary metadata lists change R vector/list storage through JSON | Compare canonical metadata content; binding validation accepts the serialized representation | Per-entry JSON/YAML provenance and plan/digest round trips |
| Concept 0 is presented as a labelled factor | Compare preserved concept identity, including independent vocabulary labels for staged chunk concatenation | Employment and full result-row oracle tests; memory/staged parity |
| Shared demo is too small or lacks inputs for some default analyses | Preserve native refusals and verify rollback; exercise complete definitions in isolated synthetic CDMs | `test-recipe-library-demo.R` and `test-recipe-library-execution.R` |

## Test scope and results

Final verification uses implementation commit `3ca1252` with matching server
`18b096ffa72feb27dd7f99fcc2cf1ab43a10dae9`. Only these ten targeted files run:

- `test-recipe-library.R`
- `test-recipe-library-execution.R`
- `test-recipe-library-boundaries.R`
- `test-recipe-library-demo.R`
- `test-v2-params.R`
- `test-v2-merge.R`
- `test-v2-merge-deferred-base.R`
- `test-v2-simple-binding-review.R`
- `test-v2-execution.R`
- `test-v2-acceptance.R`

The final targeted run passed **5,852 expectations; zero failures, errors or
skips**. Evidence: `evidence/vendors-demo-dependencies-3ca1252.log`.

The earlier SQLite plus live-demo verification of the same implementation
passed **1,761 expectations, zero failures/errors/skips** across the four
library test files. Final vendor execution covers 24 definitions on each of
SQLite, PostgreSQL 14.24, MariaDB 10.6.23 and MySQL 8.4.11: **96 template/vendor
cases**, each exercising both memory and staged publication. The live demo
adds 24 explicitly classified attempts.

`inst/recipe-library/evidence/test-run.json` records the exact filter,
environment, test files and full commit IDs; `runtime-versions.json` records
R/package/database versions. The compact evidence logs preserve execution IDs,
fixture/vocabulary checksums, demo outcomes and the test runner result.
Documentation was regenerated with roxygen2 7.3.3, and the new vignette knitted
successfully (`documentation.log`). No test fixtures regenerate during normal
test execution. Per-entry manifests and this commit-pinned run manifest are
used together to identify the execution environment.

The execution harness calls the public `recipe_execute()` path through DSLite
and inspects server-session frames only inside tests. It does not add an
analyst row-retrieval API. Each final component must meet k=5. It checks exact
component inventory, full column order/contracts, protected person keys,
independently calculated analysis rows, and memory/staged value parity.

The synthetic CDM has 24 people and explicit observations, repeated fills,
outcomes, measurements, visits and death records. Default oracle examples:
recorded coverage 44 days versus stockpiled effective coverage 60; adherence
observed denominator 365; last coded education answer missing with count two;
employment concept zero; baseline numeric value four; first outcome day ten;
recurrent event-free sentinels; dense zero-event periods; 12 cases with four
controls each and weight three. Matching key relationships are checked
separately from the analysis-row oracle. Nonzero score fixtures independently
pin age/sex, condition contributions, duplicates and post-index exclusion.

Boundary tests also cover two-site row-order/source-availability differences,
all-site rollback, explicit small-site skipping, required-table errors that
cannot be skipped, empty private features versus empty final event logs,
empty publication roots, matching seed sensitivity/reordering/candidate-cap
refusal, and zero-washout expansion. This original implementation run covered defaults and selected boundaries.
Review R2 subsequently required the complete per-entry variant bundles. The
follow-up is documented in `acceptance-matrix.md` and `library-fix1.report.md`;
the original run below is not evidence for those later variants.

Fixture table SHA-256:
`923ded40b9c9ec27ccad88062e98f8609fc8358fceb99b276b46f5e5716bed24`.
Vocabulary SHA-256:
`b4c8478eadf088f91c7d22c02c373b393a957784f7de10dcbf83c8c17604b69b`.
Each per-entry manifest also pins its expected-row checksum and server commit.
The live demo's table/vocabulary checksums are recorded separately in evidence.
No checksum is represented as a database-independent clinical validation.

## Shared demo CDM

All 24 entries are attempted through DSLite on PostgreSQL host `omopdb`,
database `omop`, schema `demo_cdm`, without modifying its data or weakening
policy. The current binding manifest uses existing demo concepts and is
visible in `test-recipe-library-demo.R`.

- Published: all six comorbidity entries, both demographics entries,
  longitudinal event log, measurement panel, all three socioeconomic entries,
  and recurrent survival (14 entries).
- Insufficient final support: active comparator, new users, nested case-control,
  incident-event cohort, first-event survival and competing risk (six entries).
- Existing factor-level disclosure refusal: incidence and SCCS panels.
- Missing required `days_supply` role: adherence components and era summary.

These are actual demo outcomes, not successful-publication claims for refused
entries. The isolated fixture provides the missing population/role coverage
and exercises all 24 definitions in memory and staged modes.

## Deviations and integration handoff

1. The three coded-concept expansions use the integrated reducer value API
   instead of the design's invalid event numeric override. This is an
   unreleased-definition correction, documented in NEWS and entry docs; no
   silently replaced published template version exists.
2. Examples use the existing named-vector `out` argument.
3. Both DESCRIPTION versions remain 2.10.4. Bumping just the client would violate
   paired-version policy, and this job expressly forbids server changes. The
   integration owner must bump both packages together for 2.10.5; catalogue
   metadata already declares its 2.10.5 target client release.
4. Full server/client suites remain the integration/release owner's gate,
   explicitly excluded by this job and COMMON.md's test-economy rule.
5. The shared demo's missing supply column and policy refusals were retained;
   no shared patients or ad hoc server configuration were inserted.
6. The merge fix is a client language correction required by the library's
   existing A-23 composition contract. No wire, normaliser or digest protocol
   changed. Other jobs using deferred panel sources should retain it.

Rd generation used roxygen2 7.3.3 on the pod. This job's pod package mirrors
and private installed library were removed after evidence capture; the full
runner log remains at its standard `.last.log` path. No other worktree or
reference tree was modified. The requested external report is an identical
copy of this committed report.
