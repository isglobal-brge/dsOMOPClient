# Categorical review fix 1 — implementation report

14 November 2026. Worktrees: `/workspace/dsomop/wt/cats/dsOMOP` and
`/workspace/dsomop/wt/cats/dsOMOPClient`, branch `feat/2.10.5-cats`.

R1–R3 of `design/2.10.3/astra-cats-review-1.md` are fixed and committed.
All final targeted checks passed; results are recorded below. Independent re-review and the
integration/release full-suite pass remain separate.

## Findings and fixes

| Finding | Implementation | Regression evidence |
|---|---|---|
| R1: prospective codebooks omit expanded category reducer columns | Shared native `.pnv2VariableColumns()` supplies the actual scalar/vector nth expansion to execution, validation and prospective schema construction. Output selection resolves expanded keys to their owning variable; native projection preserves the requested expanded-key order. Legacy preset validation retains its existing route. | `test-category-review-fixes.R`: public scalar nth, vector nth in nonascending order, one expanded-column selection and reordered expanded selection, plus broadcast-long structural-column/row preservation; prospective names/order/types compared with published native metadata. |
| R2: prospective types ignore effective presentation | Resolve numeric defaults with `.numberEffective()` only for eligible analytic values; honor overrides/resets and untranslated concepts. Share the existing pure date result-type helper between execution and the client. Apply effective category presentation to known public producer domains with `.categoryResolveRecords()`, including order, recodes, references, contrasts and sentinels. Consumed operands do not inherit final-output defaults. Integer/logical resets carry no factor contract. Vocabulary/observed domain completeness stays unresolved. | `test-category-review-fixes.R`: recipe numeric bins, explicit replacement breaks/labels, number and rounding resets, date defaults/overrides/reset, date parts/durations and numeric presentation, binary defaults/overrides/reset, category order/reset/recode/sentinels, native untranslated person concepts. Prospective and published names/types/domains/order/reference/contrasts are compared where statically declared. DSI calls are mocked to fail during both codebook operations, preserving client-only construction. |
| R3: irrelevant hierarchy defaults reject valid binary output | Preflight and presentation identify the input producer before applying vocabulary fields. Recipe hierarchy defaults are inactive for flags and numeric/date categories, including multi-target defaults and nonexistent public domains. Explicit incompatible fields still refuse; explicit scalar multi-target concept hierarchies still require indicators. Retained concept identities in mixed recodes keep vocabulary styling. | `test-category-review-fixes.R`: original bare binary control, integer/logical/factor controls, ordinary numeric and numeric/date factors, values and metadata, unavailable hierarchy domain, explicit incompatible flag/bin/date requests and explicit scalar multi-target hierarchy refusal. Existing `test-category-values.R` protects mixed-recode styling. |

No patient data is queried by a prospective codebook. All execution comparisons
use the disposable synthetic three-site harness. No release gate, noise, key,
persistence or database transport policy changed.

## Commits

Server:

- `ef36478` — Fix inactive hierarchy defaults and native expanded-column selection.
- `e596d84` — Share pure date result typing with prospective codebooks.
- `18a9973` — Retain vocabulary styling for concept identities in mixed recodes.
- `35193b3` — Preserve structural columns when selecting expanded keys in native layouts.
- `aec5c34` — Scope expanded projection to the current output variable dependencies.

Client:

- `8559664` — Resolve prospective codebooks from effective native declarations.
- `348c847` — Cover binary resets and numeric date presentations in codebooks.
- `43ed611` — Resolve prospective category domains with shared identity semantics.
- `7bfa84c` — Assert untouched binary and numeric date values under hierarchy defaults.
- `219fb7f` — Keep factor contracts inactive on scalar presentation resets.
- `5a9a71c` — Cover expanded category selection in broadcast long output.

Final implementation heads:

- dsOMOP: `aec5c3408338387e880456e7035906845542717e`.
- dsOMOPClient: `5a9a71cbb1e60822926a7fd25d915734dea0df90`.

## Targeted validation

Every R run uses `/workspace/dsomop/tools/rtest.sh`, a nonempty filter and
`RTEST_WITH` pointing at the matching sibling worktree. Dependency libraries:
`R_LIBS_USER=/root/rt/numeric-deps:/root/rt/labels-deps`.

| Run | Code snapshot | Exact test files | Result |
|---|---|---|---|
| Initial regression development | Reviewed heads `7c04e5b` / `8aee321` plus uncommitted initial fixes | `test-category-review-fixes.R` | 62 pass, 0 assertion failures, 1 test-construction error: an explicitly named `flag` was incorrectly supplied under list key `x`. Corrected the test; not final validation. |
| Client categorical dependents | Server `ef36478`, client `8559664` | `test-category-api.R`, `test-category-codebook.R`, `test-category-federation.R`, `test-category-review-fixes.R`, `test-category-selection.R`, `test-category-values.R`, `test-category-vocabulary.R` | 534 pass, 0 assertion failures, 1 error, 0 skips. Mixed-recode styling was over-restricted in the first implementation; corrected in `18a9973`. The review regressions passed. |
| Client presentation verification | Server `18a9973`, client `219fb7f` | `test-category-review-fixes.R`, `test-category-codebook.R`, `test-category-values.R`, `test-number-system-api.R`, `test-v2-plan.R`, `test-ws0-normalize.R` | **578 pass, 0 failures/errors/skips.** |
| Server compiler/backend verification | Server `18a9973`, client `219fb7f` | `test-category-contract.R`, `test-category-wire.R`, `test-category-backends.R`, `test-compile-reduce.R`, `test-compile-output.R`, `test-date-system-outputs.R`, `test-ws0-normalize.R` | **2,721 pass, 0 failures/errors/skips.** SQLite, DuckDB, PostgreSQL, MariaDB and MySQL; memory and actual Parquet. |
| Expanded projection regression | Server `35193b3`, client `5a9a71c` | `test-category-review-fixes.R` | **116 pass, 0 failures/errors/skips.** |
| Output compiler follow-up | Server `aec5c34`, client `5a9a71c` | `test-compile-output.R` | **766 pass, 0 failures/errors/skips.** |

Evidence is retained next to this report: `cats-fix1-first.log` and `.full.log`,
`cats-fix1-client.log` and `.full.log`, `cats-fix1-client-final.log` and `.full.log`, `cats-fix1-server-final.log`
and `.full.log`, `cats-fix1-client-projection.log` and `.full.log`, and
`cats-fix1-server-projection.log` and `.full.log`.
The first two runs are disclosed development results, not claimed as passes.
The final client log retains 33 existing rollback-cleanup warnings about missing
temporary execution symbols, all in expected-refusal cases; no successful
execution case failed. No warnings were suppressed or tests disabled.

Exact filters/timeouts:

- Initial: `^category-review-fixes$`, 900 seconds.
- Client dependents: `^category-(review-fixes|codebook|api|values|federation|selection|vocabulary)$`, 1800 seconds.
- Final client: `^(category-(review-fixes|codebook|values)|number-system-api|v2-plan|ws0-normalize)$`, 1200 seconds.
- Final server: `^(category-(contract|wire|backends)|compile-(reduce|output)|date-system-outputs|ws0-normalize)$`, 1200 seconds.
- Expanded projection client follow-up: `^category-review-fixes$`, 900 seconds.
- Output compiler server follow-up: `^compile-output$`, 900 seconds.

Server vendor settings: `DSOMOP_TEST_POSTGRES_HOST=omopdb`,
`DSOMOP_TEST_MARIADB_HOST=127.0.0.1`, `DSOMOP_TEST_MYSQL_HOST=127.0.0.1`,
`DSOMOP_TEST_MYSQL_PORT=3307`.

Independent Python contract check on `18a9973` / `219fb7f` (the later projection-only changes do not alter its inputs):
`PYTHONDONTWRITEBYTECODE=1 python3 wt/cats/dsOMOP/tools/ws0/check_contracts.py wt/cats/dsOMOP wt/cats/dsOMOPClient`.
Passed 3 schemas, 2 examples, 224 probes, 15 acceptance plans, 12 digest vectors,
18 normalized fixtures, 34 numeric fixtures and 11,043 double-corpus cases,
including the category loops. Evidence: `cats-fix1-contracts-final.log`.

## Scope and handoff

The fixes implement the approved contract; no design amendment was needed.
Expanded-key selection required corresponding native client/server validation
and projection changes, because validation previously admitted only variable
names. Final source review also extended that projection beyond wide to native
layouts and scoped it to each output's variable dependencies, retaining source
and structural columns. The long-output regression and compiler follow-up cover
this final change. Date-type inference was shared to prevent drift from execution.

Both packages retain version 2.10.5. The shared `category_semantics.R`,
`category_resolution.R`, `number_semantics.R`, `date_semantics.R`,
`concept_labels.R` and `plan_normalize_v2.R` are byte-identical. Keep the paired
shared-file changes when integrating; `.dateResultType()` now lives in shared
`date_semantics.R`, with unchanged server semantics. NEWS documents the
corrected behavior and the explicit hierarchy alternative.

Only targeted tests were used, following the later test-economy rule in
COMMON.md and the user's explicit instruction. No full suites, Claude CLI,
sub-agents, pushes, PRs, tags or stashes were used. No public signatures changed,
so no roxygen regeneration was required.

`git diff --check` passed in both worktrees. Local free space was 9.8 GB at
start and 9.7 GB at completion. CLEANUP.md was read; no full suite or large
local copy was performed and no unrelated files were deleted. No local scratch
package copies were created.

After preserving the evidence, removed only this job's disposable pod mirrors
and private package libraries. Shared fixtures, dependency libraries and runner
logs were retained. Sizes measured immediately before deletion
(`cats-fix1-cleanup.log`):

| Deleted pod path | Bytes |
|---|---:|
| `/root/rt/wt/cats/dsOMOP` | 24,745,777 |
| `/root/rt/wt/cats/dsOMOPClient` | 15,596,657 |
| `/root/rt/lib/wt_cats_dsOMOP` | 5,622,182 |
| `/root/rt/lib/wt_cats_dsOMOPClient` | 11,776,420 |

The requested report is outside a Git repository. An identical copy is committed
in the client at `tools/categories/cats-fix1.report.md`; all job logs are archived
at `tools/categories/cats-fix1-evidence/` (trailing whitespace removed only;
original logs remain beside this report). That documentation/evidence commit
changes no tested implementation or tests.
