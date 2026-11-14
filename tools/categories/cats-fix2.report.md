# Categorical review fix 2 — implementation report

14 November 2026. Worktrees: `/workspace/dsomop/wt/cats/dsOMOP` and
`/workspace/dsomop/wt/cats/dsOMOPClient`, branch `feat/2.10.5-cats`.
Starting heads were the exact reviewed heads `aec5c34` / `bd9f385`;
both worktrees were clean. Both package versions remain 2.10.5.

## Findings and repairs

| Finding | Fix | Targeted regression evidence |
|---|---|---|
| R4 — prospective output-local copies ignore final selection/order | Build component and copy declarations before final projection. The client and server share the pure `.categorySelectedColumns()` helper, which expands variable selections in place and retains structural keys. Apply final selection to copies as well as originals, including ordinary selections and whole expanded variables. Joined components distinguish inherited analytic variables from structural columns; reference companions retain their structural schema. Resolve an expanded copy dependency such as `answer_1` to its owning variable before compilation, even when only the copy is selected. Numeric preflight recognizes selected category-copy names when both addition maps coexist. | `test-category-review-composition.R`: excluded copies, copy-only selections, ordinary and expanded interleaving, whole-variable expansion, mixed number/category copies, joined-component interleaving/copy-only selections, and public native category-nth selection. Prospective and published names/order are compared after DSI aggregate/assign calls are mocked to fail, proving codebook construction stays local. Existing `test-category-review-fixes.R` covers scalar/vector nth and long-output structural retention; `test-category-columns.R` covers reference copies. |
| R5 — categorical selector reductions cannot be copied | The compiler marks only categorical selector result descriptors with `category_result`; numeric reductions retain their numeric type. Original-frame binding accepts that declared result type and copies the existing value and descriptor intact, including `category_consumed`, input lineage, availability, definedness and upstream admission. Existing presentation consumes that contract, without replaying input identity operations. Arbitrary text/factors and numeric quantities remain ineligible. | `test-category-review-composition.R`: unwrapped scalar mode/vector nth; copied values and full contracts equal originals on three sites; independent ordered presentation; consumed recodes; Missing/Unknown sentinels; absent nth cells; memory and actual Parquet; identical harmonized factor contracts across sites; unsafe upstream identity refused even when only the copy is selected; unavailable/undefined originals and independently presented copies stay NA. `test-category-contract.R` rejects plain text, observed factors, numeric cells and input-lineage-only descriptors. |

The numeric preflight and expanded dependency repairs are necessary parts of R4's
required copy composition. NEWS in both packages classifies the fixes as
corrected behavior and gives the migration path: re-run affected native recipes,
with no declaration change. No new wire capability or version increment is needed.

## Commits

Server:

- `e13341b` — Keep inherited variable columns out of structural copy projection.
- `6e821a77fa727b6984c6f532645e056364c825e7` — Fix final copy selection and typed categorical reduction copies.
- `52f47dca35236fa9f42e59fe667315b2e03b9291` — Resolve expanded copy dependencies before output projection.

Client:

- `521aa1b` — Apply copy projection to inherited joined component declarations.
- `6b4c95745d2b2ec7fbdc3b1a468146385d79901f` — Project prospective copy schemas and cover reduced category composition.
- `7fb3aaab1d6ef6d9691741e0484a03d0b09a8d34` — Use complete identity ordering for consumed-copy presentation test.
- `9eb17d6bc295b55d90357d751074125c54e76595` — Cover unwrapped copies and unavailable reduced-category lineage.

The final documentation commit archives this report and exact compressed evidence
under client `tools/categories/`; implementation heads are server `e13341b` and
client `521aa1b`.

## Targeted validation

Every R invocation used `/workspace/dsomop/tools/rtest.sh`, a nonempty filter,
a 1,200-second timeout, and `RTEST_WITH` pointing to the matching sibling worktree.
All used `R_LIBS_USER=/root/rt/numeric-deps:/root/rt/labels-deps`. Server backend
checks additionally used `DSOMOP_TEST_POSTGRES_HOST=omopdb`,
`DSOMOP_TEST_MARIADB_HOST=127.0.0.1`, `DSOMOP_TEST_MYSQL_HOST=127.0.0.1`,
and `DSOMOP_TEST_MYSQL_PORT=3307`.

| Run | Server/client snapshots | Exact test files | Result |
|---|---|---|---|
| Initial regression development | `6e821a7` / `6b4c957` | `test-category-review-composition.R` | 13 passed, 0 assertion failures, 2 errors: expanded-copy owner was not compiled; independent presentation test supplied an incomplete explicit order. Fixed dependency resolution and corrected the test to use the complete identity order. |
| Composition verification | `52f47dc` / `7fb3aaa` | `test-category-review-composition.R` | **129 passed; 0 failures/errors/skips.** |
| Server compiler and backend dependents | `52f47dc` / `7fb3aaa` | `test-category-contract.R`, `test-category-wire.R`, `test-category-backends.R`, `test-compile-output.R`, `test-number-system-outputs.R` | **1,305 passed; 0 failures/errors/skips.** SQLite, DuckDB, PostgreSQL, MariaDB and MySQL; memory and Parquet. |
| Direct IR dependency checks | `52f47dc` / `9eb17d6` | `test-ir.R` | **776 passed; 0 failures/errors/skips.** |
| Client categorical dependents | `52f47dc` / `9eb17d6` | `test-category-review-composition.R`, `test-category-review-fixes.R`, `test-category-codebook.R`, `test-category-values.R`, `test-category-columns.R`, `test-category-missing-lineage.R` | **610 passed; 0 failures/errors/skips.** |
| Joined projection compiler follow-up | `e13341b` / `521aa1b` | `test-compile-output.R` | **528 passed; 0 failures/errors/skips.** This follow-up used SQLite/DuckDB, without the vendor-host environment used in the earlier backend pass. |
| Final composition verification | `e13341b` / `521aa1b` | `test-category-review-composition.R` | **189 passed; 0 failures/errors/skips.** Includes joined selections, unavailable/undefined copies, memory and Parquet. |

Filters were respectively `^category-review-composition$` (development and
composition follow-ups),
`^(category-(contract|wire|backends)|compile-output|number-system-outputs)$`,
`^ir$`,
`^category-(review-composition|review-fixes|codebook|values|columns|missing-lineage)$`,
and `^compile-output$`.

The final Python contract checker ran against both final implementation heads:
`PYTHONDONTWRITEBYTECODE=1 python3 wt/cats/dsOMOP/tools/ws0/check_contracts.py wt/cats/dsOMOP wt/cats/dsOMOPClient`.
It passed 3 schemas, 2 examples, 224 probes, 15 acceptance plans, 12 digest
vectors, 18 normalized fixtures, 34 number fixtures and 11,043 double-corpus
cases. The six shared files `category_semantics.R`, `category_resolution.R`,
`number_semantics.R`, `date_semantics.R`, `concept_labels.R` and
`plan_normalize_v2.R` remain byte-identical. Both worktrees pass `git diff --check`.

The client dependent run retains 60 rollback-cleanup warnings about missing
execution-owned temporary symbols in expected-refusal tests. The 129-assertion and final 189-assertion
composition runs each retain three such warnings from their upstream-admission refusal.
Server runs have no testthat warnings. No warnings were suppressed, tests disabled
or full suites invoked. The initial failed development run is retained as failed
evidence, not represented as a pass.

Evidence is retained next to this report as `cats-fix2-{first,second,client,server,ir,output-final,composition-final}.log`
and corresponding `.full.log` files, plus the contract-check and cleanup logs.
Exact gzip-compressed copies and SHA-256 checksums are committed under client
`tools/categories/cats-fix2-evidence/`; no raw log whitespace is rewritten.

## Scope, deviations and handoff

The later test-economy rule in COMMON.md and the explicit task instruction govern:
only changed-code tests and direct dependents ran. Integration/release owns the
full-suite pass; independent review remains a separate task. No design amendment
was needed. Other jobs should retain the shared projection helper in both
packages and the selector-only result marker when merging these commits.

No database TLS requirement, privacy gate, persistence, quota, noise, key or
per-user behavior changed. All executions use disposable synthetic fixtures.
No push, PR, tag, stash, sub-agent or Claude CLI was used. Only the named
worktrees and this job's requested design artifacts were edited.

Disk stayed at approximately 9.7 GB free. CLEANUP.md was read; no large local
copies were made and no unrelated files were deleted. The inspected local
scratch/cache candidates yielded no eligible cleanup. The remaining low free
space is shared; this job did not remove other worktrees or evidence to reach
the aspirational 15 GB target. Its own disposable pod sources and private test
libraries are removed after workers finish and logs are preserved; exact paths
and measured sizes are recorded below and in `cats-fix2-cleanup.log`.

| Deleted pod path | Bytes before deletion |
|---|---:|
| `/root/rt/wt/cats/dsOMOP` | 24528243 |
| `/root/rt/wt/cats/dsOMOPClient` | 16114692 |
| `/root/rt/lib/wt_cats_dsOMOP` | 5611232 |
| `/root/rt/lib/wt_cats_dsOMOPClient` | 11723861 |

R4 and R5 are fixed. All final targeted checks passed.
