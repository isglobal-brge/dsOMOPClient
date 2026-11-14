# Categorical and layout implementation — CR-31 / A-30

Implementation and targeted developer validation are complete. All C01–C65 acceptance cases are covered, with no unresolved failures in the selected files after the documented fixes and focused reruns. Final focused results: server **9,712 passed** and client **606 passed**, both with zero failures, errors or skips. Source heads: dsOMOP `7c04e5b`, dsOMOPClient `b7aac43`; the final client commit adds this report only.

## Scope

Implemented the approved A-30 amendment and CR-31 acceptance inventory C01–C65, including the independent review's exact edits. Work is confined to the two `feat/2.10.5-cats` worktrees and this job's reports/logs. Both packages are version 2.10.5. No Claude CLI, delegation, pushes, PRs, tags or stashes.

The worktrees were fast-forwarded to matching A-29 dependency heads: server `948e5fe`, client `befc968`. Independent A-29 certification remains a release prerequisite; the targeted numeric regressions here cover direct dependencies of this change.

## Implemented behavior

- Closed public declarations for category order/reference, nominal recodes, vocabulary hierarchy rollups, label styles/language, Missing/Unknown and integer/logical/factor flags.
- Explicit event category projections, indicators and category reductions; preserved source admission, alias equivalences, typed mode ties and consumed identity semantics.
- Declared time points and visit ordinals with canonical shared-row selection, calendar offsets, reuse policy, empty/unavailable distinctions, broadcast and collision validation.
- Output-local category copies, typed nested/reference components, all-table prospective and published codebooks, and complete memory/Parquet factor contracts.
- Required native capabilities, closed schemas, deterministic normal forms/digests, v1 refusals, federation reconciliation and atomic rollback.

Public usage examples are in `dsOMOPClient/tools/categories/usage.md`; generated help covers `omop_category_spec`, `omop_category_reduce`, `omop_repeated` and `recipe_codebook`.

## Findings, fixes and focused evidence

| Finding | Fix | Test files (`test-*.R`) |
|---|---|---|
| Nullable labels/references lost their positions or JSON types | Null-preserving metadata encoding, plain hash strings, named alias maps and reference normalization | category-values, category-codebook, category-contract, category-wire |
| Nested reduction lost consumed identities during row alignment | Preserve the consumed contract and subset its private identity vector alongside rows | category-reducers, category-values |
| Reduced nominal categories reused vocabulary names and lost admitted aliases | Bind effective nominal labels, preserve upstream admission and carry original equivalence keys | category-reducers, category-values, category-federation |
| An explicit outer vocabulary stage operated on the original input or refused consumed concepts | Apply style/rollup to the retained effective concept, preserving the admission keys | category-vocabulary |
| Public recode inputs or rollup sources absent at one site changed the consumed contract | Declare recode keys and all valid vocabulary-map sources independently of local observations | category-values, category-vocabulary |
| Explicit recode labels retaining concept identities were overwritten by outer automatic styles | Preserve recode provenance across reduction and exclude explicit labels from automatic restyling | category-values |
| New public arguments invalidated an older positional-formals assertion | Extend the expected tail to include number_spec and category_spec | concept-label-api |
| Legacy output `derived` strings entered the native category walker through partial matching | Use exact lookup and visit only list-valued derived declarations | concept-label-legacy |
| Native normalized capability inventories omitted codebook metadata while legacy helpers could acquire it | Classify native IR explicitly using source version; negotiate metadata only for native release paths | v2-capability-criteria, v2-plan, v2-review, concept-label-legacy |
| Empty and unavailable selections could be confused after presentation | Carry private availability/anchor masks and canonical pre-presentation missingness | category-missing-lineage, category-selection, category-repeated, category-visits, category-contract |
| Repeated numeric siblings could select different event universes | Reject incompatible variable-local source-unit streams with shared-event or separate-bundle remedy | category-streams |
| Open periods and public numeric grids risked truncated domains | Keep admitted open-domain values plus declared unused levels; reject incomplete explicit orders | category-federation |
| Mixed concept-zero equivalences could reclassify known concepts | Structural refusal unless the whole inherited group is recoded; explicit pre-merge zero recode stays ordinary | category-values |
| Hierarchy ambiguity, malformed adapters and graph cycles require public-input decisions | Exact vocabulary classes/edges/distances, public-domain preflight and bounded errors | category-vocabulary |
| Default output selection consumed unrelated variables or dropped additions | Bind original output columns once, retain selected nested covariates and category copies | category-columns, category-layout, category-cdm |
| Prospective/released books missed fixed components, units or untranslated outputs | Pure component allocation and declared lineage/types; universally negotiated native release metadata | category-codebook, category-layout, category-cdm |
| Support can differ by accessible component and retained site | Preserve final per-component gates, retained metadata sets and family rollback | category-federation |
| Factor storage codes and session contrasts cannot define federated analysis | Explicit shared identities/reference/contrasts and actual protected endpoint comparisons | category-analysis, category-vocabulary, category-cdm |

## Acceptance coverage index

Each row identifies the executable cases carrying that acceptance ID. Synthetic fixtures supply exact counterexamples; the retained GiBleed/Synthea matrices exercise real CDM execution and output transport. This is not a claim that each synthetic vocabulary edge exists naturally in both datasets.

| ID | Test files |
|---|---|
| C01 | category-api, category-federation |
| C02 | category-api, category-values, category-wire |
| C03 | category-api |
| C04 | category-federation |
| C05 | category-reducers, category-values |
| C06 | category-values |
| C07 | category-federation |
| C08 | category-analysis, category-federation |
| C09 | category-vocabulary |
| C10 | category-vocabulary |
| C11 | category-vocabulary |
| C12 | category-vocabulary |
| C13 | category-vocabulary |
| C14 | category-vocabulary |
| C15 | category-vocabulary |
| C16 | category-vocabulary |
| C17 | category-vocabulary |
| C18 | category-reducers, category-values |
| C19 | category-reducers, category-vocabulary |
| C20 | category-vocabulary |
| C21 | category-vocabulary |
| C22 | category-vocabulary |
| C23 | category-vocabulary |
| C24 | category-vocabulary |
| C25 | category-vocabulary |
| C26 | category-values |
| C27 | category-values |
| C28 | category-contract, category-missing-lineage |
| C29 | category-backends, category-federation, category-missing-lineage |
| C30 | category-missing-lineage |
| C31 | category-missing-lineage |
| C32 | category-backends, category-contract, category-repeated |
| C33 | category-contract, category-repeated |
| C34 | category-contract, category-repeated, category-selection |
| C35 | category-contract, category-repeated, category-selection |
| C36 | category-selection, category-streams |
| C37 | category-selection |
| C38 | category-contract, category-repeated, category-selection |
| C39 | category-visits |
| C40 | category-visits |
| C41 | category-visits |
| C42 | category-repeated, category-visits |
| C43 | category-selection |
| C44 | category-repeated |
| C45 | category-columns |
| C46 | category-backends, category-cdm, category-cdm-server, category-layout |
| C47 | category-columns |
| C48 | category-backends, category-cdm, category-codebook, category-federation, category-layout |
| C49 | category-cdm-server, category-contract, category-federation |
| C50 | category-federation |
| C51 | category-codebook, category-vocabulary |
| C52 | category-federation |
| C53 | category-cdm, category-codebook, category-layout |
| C54 | category-cdm, category-codebook, category-layout |
| C55 | category-codebook, category-federation |
| C56 | category-cdm, category-missing-lineage |
| C57 | category-api, category-reducers, category-wire |
| C58 | category-api, category-wire |
| C59 | category-api, category-codebook |
| C60 | category-analysis, category-vocabulary |
| C61 | category-federation, category-missing-lineage |
| C62 | category-vocabulary |
| C63 | category-reducers |
| C64 | category-cdm, category-cdm-server, category-values |
| C65 | category-values |

## Validation ledger

The combined client run exceeded its initial 2,400-second shell-watchdog estimate. Only that self-imposed watchdog was paused while the R worker continued unchanged; a separate supervisor resumed it after R exited. Both supervisor and test runner completed. No test or assertion was skipped by this extension.

Only targeted tests were run, using `/workspace/dsomop/tools/rtest.sh` with `RTEST_WITH` set to the matching sibling. The later test-economy rule in COMMON.md and the user's explicit instruction override the older full-suite text. Integration/release owns the eventual full-suite pass.

| Evidence | Server / client source heads | Selected test files | Result |
|---|---|---|---|
| `cats-server-fourteenth.log` | `58304ea` / `d7e6a7e` | category-backends, category-cdm-server, category-contract, category-wire; concept-label-contract, concept-label-presentation; number-system-wire, number-system-outputs; ws0-schema, ws0-normalize | 1,532 pass; 0 failures/errors; 1 optional Python-jsonschema skip |
| `cats-server-fifteenth.log` | `9c8a15f` / `fd699d8` | category-backends, category-cdm-server, category-contract, category-wire | 264 pass; 0 failures/errors/skips |
| `cats-twentyeighth.log` | `58304ea` / `d7e6a7e` | category-cdm, category-values, category-reducers | Intentionally interrupted during GiBleed reconciliation after publication; no pass claimed. The ten-year panel fixture was reduced to one period without removing people or output families, then rerun with per-component progress. |
| `cats-thirtieth.log` | `9c8a15f` / `c443f26` | category-analysis, category-api, category-cdm, category-codebook, category-columns, category-federation, category-layout, category-missing-lineage, category-reducers, category-repeated, category-selection, category-streams, category-values, category-visits, category-vocabulary; concept-label-api/contract/federation/analysis/legacy; number-system-api/wire/federation; ws0-schema/normalize; v2-capability-criteria/execution/plan/review | 3,564 pass; 27 failures, 3 errors. Both CDM matrices and all category files except the outer ATC rollup passed. Follow-up fixes and reruns below resolve the failures. |
| `cats-server-sixteenth.log` / `cats-server-sixteenth.full.log` | `e891aa7` / `72c0f80` | category-backends/cdm-server/contract/wire; concept-label-contract/presentation; explain; ir; ir-corpus; ir-v1-compat; ws0-schema/normalize | 13,587 pass; 824 failures, 0 errors, 1 optional Python-jsonschema skip. 822 v1 canonical-result mismatches and one legacy presentation attribute mismatch caused by the subsequently corrected native/legacy metadata classification; one stale capability inventory assertion corrected to enumerate A-29/A-30 operations. Frozen IR corpus, schema, normalization and all category files passed. |
| `cats-thirtyfirst.log` / `cats-thirtyfirst.full.log` | `e891aa7` / `72c0f80` | category-analysis/missing-lineage/values/reducers/vocabulary; concept-label-api/legacy; v2-capability-criteria/plan/review | 705 pass; 0 failures, 2 errors: bounded-refusal test text and legacy helper metadata classification, both corrected before the final rerun. |
| `cats-thirtysecond.log` / `cats-thirtysecond.full.log` | `f0acb2c` / `b7aac43` | category-missing-lineage, category-values, category-vocabulary, category-codebook; concept-label-legacy; v2-capability-criteria, v2-plan, v2-review | 606 pass; 0 failures/errors/skips. Resolves the remaining client failures and checks recode contracts when a public source identity is absent locally. |
| `cats-server-seventeenth.log` / `cats-server-seventeenth.full.log` | `7c04e5b` / `b7aac43` | category-backends, category-cdm-server, category-contract, category-wire; concept-label-presentation; explain; ir-v1-compat | 9,712 pass; 0 failures/errors/skips. Resolves all server-run-16 failures, including the complete frozen v1 execution compatibility replay. |
| `cats-contracts-final.log` | `f0acb2c` / `b7aac43` | Independent Python checker | Passed: 3 schemas, 2 examples, 224 probes, 15 acceptance plans, 12 digest vectors, 18 normal forms, 34 number fixtures, 11,043 double corpus cases; also 14 category vectors and 19 category schema probes |

`cats-twentyninth.log` was also intentionally interrupted after full-cohort memory reconciliation and during staged long reconciliation. Per-component progress identified the expensive path; `c443f26` bounds only the client validation cohort to 108 fixed roster IDs. The earlier full cohort and ten-year panel attempts are not counted as completed passes.

Earlier numbered logs are iterative debugging evidence, not certification of the final heads. Full detailed logs for client runs 30–32 and server runs 16–17 are retained alongside this report. The full CDM matrix passed in run 30; subsequent runs cover every affected file and the fixes made afterward. No final full-suite claim is made. Public Rd/NAMESPACE were generated on the pod with roxygen2 7.3.3 (`cats-roxygen.log`). Later changes are internal code/tests and do not change public formals.

Shared `category_semantics.R`, `category_resolution.R`, `number_semantics.R`, `concept_labels.R` and `plan_normalize_v2.R` are byte-identical between packages. Both worktrees pass `git diff --check`; all public help was generated with roxygen2 7.3.3.

## Dataset and backend evidence

- GiBleed: retained 5.3 archive SHA-256 `081b82e84c90e2904e02c26546e46412b01cce566071c77e0c4a355c2d0612f9`; loaded SQLite SHA-256 `1de36fcf08a07d198d8b021ec25e39f0b59ca18939d884b0caa474cc114e59e2`. Fixture `/root/rt/cats-data/gibleed.sqlite`, 2,694 people. The client harness uses the first 108 roster IDs (ordered by person_id), independent of clinical values, split into three disposable disjoint person partitions. Full-cohort server checks use the retained CDM. Numeric measurement values may all be NA; counts and typed unused domains remain valid.
- Synthea27Nj: pinned upstream revision `3d349dcdd79cc346b8e36eb837e49feffe7f7497`, archive SHA-256 `cdd3971be325e17b0e872bf4560711ab4c346917ecb079e2faf32badb55cb693`. Retained PostgreSQL `numeric_cdm` and DuckDB fixture; three disjoint PostgreSQL person views for the client, with 28 people overall.
- Public all-format recipe: 12 named outputs (including baseline/features presets), 10 native formats and 15 real components, including sparse/temporal reference companions. Additional fixtures cover all five survival forms: ordinary survival, competing risk, counting process, recurrent events and multi-state, including their reference companions, generated panel/reference category copies and unsupported matrix-cell refusals.
- Server backend tests: SQLite, DuckDB, PostgreSQL 14, MariaDB 10.6 and MySQL 8.4.11, memory and actual Parquet. Chunk assertions inspect full levels, ordered class and contrasts before any test-harness row binding.
- Protected tables compare pooled results with centralized synthetic harness oracles and reversed connection order. Actual dsBaseClient GLMs compare nominal education, ordinal education, ordered numeric bins, integer/logical/factor flags and overlapping hierarchy flags across two and three sites against centralized fits. Model warnings from constant synthetic outcomes do not substitute for these coefficient assertions.

## Deliberate boundaries and integration notes

- Native releases require `output.codebook_metadata`, including plain numeric/untranslated plans. Upgrade participating servers together to 2.10.5; legacy v1 remains in its compatibility route. New constructors refuse explicit v1 rendering by name.
- No independently admitted class-1 source-concept route exists in the current package. `source_code` therefore refuses with a bounded remedy; it never reconstructs source text from standard concepts. This follows A-30's conditional source-code admission.
- Existing protected table/model APIs use in-memory frames. This change preserves their admission rules; staged Parquet retains the full typed contract for staged consumers and is not silently passed to an in-memory-only endpoint.
- No new owner configuration, TLS requirement, quotas, privacy accountant, user-specific policy or durable catalogue was introduced. Only existing final publication/aggregate gates apply.
- Independent coordinator/reviewer implementation approval and the integration full-suite pass remain separate from this developer report.
- `/workspace` had approximately 9.8 GB free. CLEANUP.md was read; no large local copies/downloads or unrelated deletions were made. Test fixture copies live on the pod and their temporary partitions are cleaned by the harness. No scratch package copies remain in the workspace. Removed this job's regenerable `/workspace/dsomop/wt/cats/dsOMOP/tools/ws0/__pycache__/`: 88 KiB allocated, 75,016 file bytes.

After saving the evidence logs, removed only this job's regenerable pod package mirrors and private test libraries:

| Deleted pod path | File bytes (`du -sb`) |
|---|---:|
| `/root/rt/wt/cats/dsOMOP` | 24,741,687 |
| `/root/rt/wt/cats/dsOMOPClient` | 15,555,689 |
| `/root/rt/lib/wt_cats_dsOMOP` | 5,609,841 |
| `/root/rt/lib/wt_cats_dsOMOPClient` | 11,770,162 |

Retained the CDM fixtures, runner logs, shared dependency libraries and all review evidence. No other job's files were removed.

## Commits

### dsOMOP

- `06fcc3a` — Implement A-30 category identity and repeated layout contracts
- `52755bd` — Harden category contracts, typed transport and selection semantics
- `2c1ce7e` — Preserve category lineage and extend release and layout contracts
- `65b80f6` — Preserve inherited category aliases and codebook contracts across releases
- `d98bece` — Describe declared units and retain admitted aliases in codebooks
- `cfc7c49` — Carry category reduction identity and admission through row alignment
- `58304ea` — Preserve admitted aliases and effective nominal labels through reductions
- `9c8a15f` — Apply outer vocabulary stages to retained category identities
- `fd1bdbe` — Retain explicit labels for concept identities after category reduction
- `24e6b90` — Keep consumed recode labels ahead of vocabulary styling
- `e891aa7` — Make public hierarchy contracts and native capability inventories site invariant
- `31b321f` — Distinguish native codebook releases from legacy presentation helpers
- `f0acb2c` — Declare recode input identities independently of local observations
- `7c04e5b` — Assert amended numeric and categorical capability inventory

### dsOMOPClient

- `fd9b594` — Implement A-30 category identity and repeated layout contracts
- `3228b4b` — Harden category contracts, typed transport and selection semantics
- `d11ece7` — Expand codebook schemas and categorical acceptance coverage
- `6ba921f` — Cover merged aliases, calendar selection, component codebooks and vendor parity
- `d6c40e6` — Complete prospective types, codebook units and output-family tests
- `d7e6a7e` — Exercise inherited recodes, repeated stream refusals and protected hierarchy pooling
- `fd699d8` — Cover explicit hierarchy and style stages after category reduction
- `047b8fb` — Cover zero recodes and denied upstream category reductions
- `3549fd7` — Bound CDM period fixture and expose reconciliation progress
- `865aa71` — Explain consumed category stages and explicit outer presentation
- `a3d2a1f` — Cover remaining survival layouts and excluded ICD10 mapping edges
- `c443f26` — Use a fixed GiBleed roster cohort for targeted federation checks
- `22b4289` — Compare pooled GLMs for ordered bins and every binary representation
- `d06ff62` — Assert category source refusals and date-dependent release lineage
- `520095d` — Check retained concept labels after explicit category recodes
- `594a5c4` — Verify outer styles retain explicit reduced category labels
- `33b0d3e` — Update API append assertions for approved numeric and category specifications
- `72c0f80` — Preserve legacy derived fields and negotiate codebooks in DP test plans
- `10f002a` — Keep v1 execution receipts free of native codebook metadata
- `126127b` — Check consumed recodes when a public source identity is absent at one site
- `b7aac43` — Assert bounded source refusal and rollback at the public endpoint

The final client report-only commit records this report verbatim in `tools/categories/implementation-report.md`; the implementation/test heads above remain the validation references.
