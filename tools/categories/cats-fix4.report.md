# Categorical review 4 repair report

14 November 2026. Worktrees: `/workspace/dsomop/wt/cats/dsOMOP` and
`/workspace/dsomop/wt/cats/dsOMOPClient`, branch `feat/2.10.5-cats`.
Starting heads were the reviewed `600c280` / `2c087a0`; both were clean.
Both package versions remain 2.10.5.

## Finding mapped to repair and tests

**R7 (the sole blocking finding): corrected.** The public prospective schema now
expands native date endpoints after variable covariates and before output-copy
projection, matching execution. Both packages share `.dateOutputEndpoints()` in
`R/date_semantics.R`, including the original endpoint mapping and its order.
The server uses that helper without changing its expansion conditions or values.
The client only performs final projection when execution does (expanded keys or
output copies), preserving ordinary covariate/endpoint append order otherwise.
The client uses the effective recipe date presentation to declare each generated
column's R type. Endpoint start/end roles remain structural even for numeric date
presentation. Reference components do not gain patient endpoints. Nested survival
components are expanded individually; joined outputs inherit the resulting final
schema and types. Construction remains local.

`tests/testthat/test-category-review-schema.R` contains the exact review D1 panel
and D2 survival recipes with `omop_date_spec(precision="month")`, alongside their
no-date controls. It compares prospective and published names/order with actual
frames on all three synthetic sites in memory and actual staged Parquet. Both
codebook constructions run with DSI aggregate and assign functions mocked to
error. Generated date R types are also compared across the three representations.
The same checks cover copy-only selections, selected panel originals, all five
survival styles, recurrent event/risk-set components, multi-state reference
companions, multiple TARs and joined panel outputs. Additional projected and
unprojected survival cases check endpoint append order and Date, ordered-factor,
numeric-year and relative-day presentations.

NEWS in both packages classifies this as corrected behavior and tells users to
rebuild affected codebooks without changing recipe declarations. No new design
amendment is needed; this implements A-30/CR-31's existing composition contract.

## Commits

- Server `f5c88cecd6b441f30675f9bb14ca545b9297c0f0` — Share native date endpoint
  expansion with prospective schemas.
- Client `f8f1c855820ec28411f55ef2b885823ae17d5969` — Expand prospective date endpoints
  before categorical output projection.
- Client `59031a03dec40b6b8e544af1bef432031597cb1c` — Preserve native endpoint append order when no final projection runs.
- A final client documentation commit archives this report and the evidence under
  `tools/categories/cats-fix4.report.md` and `tools/categories/cats-fix4-evidence/`.

## Targeted verification

| Run | Exact tested commits (server / client) | Exact test files | Result |
|---|---|---|---|
| Initial R7 regressions | `f5c88ce` / `f8f1c85` | `test-category-review-schema.R` | 1,029 passed; 40 failed; 0 errors/skips. Exact D1/D2 and nested/joined tests passed. All failures were in the added no-copy survival cases: unconditional prospective projection moved endpoint columns ahead of a declared variable. `59031a0` matches execution's projection condition and fixes that ordering. Initial evidence is retained. |
| Server direct dependents | `f5c88ce` (client not imported) | `test-compile-output.R`, `test-date-system-outputs.R`, `test-number-system-outputs.R` | **1,975 passed; 0 failures/errors/skips; no testthat warnings.** |
| Final client regressions and direct dependents | `f5c88ce` / `59031a0` | `test-category-review-schema.R`, `test-category-review-composition.R`, `test-category-review-fixes.R`, `test-category-codebook.R`, `test-category-columns.R`, `test-category-layout.R` | **1,548 passed; 0 failures/errors/skips.** |

Exact filters: `^category-review-schema$`,
`^(compile-output|date-system-outputs|number-system-outputs)$`, and
`^category-(review-schema|review-composition|review-fixes|codebook|columns|layout)$`.

The successful final runs total **3,523 assertions**. The final client run retains
21 rollback-cleanup warnings about missing execution-owned temporary symbols in
expected-refusal tests, matching the warnings documented in review 4. The initial
schema run and the server run have no testthat warnings. No warnings were
suppressed and no tests were disabled.

All R tests ran through `/workspace/dsomop/tools/rtest.sh`, with nonempty filters
and explicit timeouts (1,200 seconds initially; 1,800 seconds for the combined
final client run, which includes the enlarged publication matrix and direct dependents). Client runs install the matching server via `RTEST_WITH`.
All use `R_LIBS_USER=/root/rt/numeric-deps:/root/rt/labels-deps`. Server tests also
use the prescribed PostgreSQL/MariaDB/MySQL hosts and MySQL port 3307.

The Python contract checker passed at the implementation commits above: 3 schemas,
2 examples, 224 probes, 15 acceptance plans, 12 digest vectors, 18 normalized
fixtures, 34 number fixtures and 11,043 double-corpus cases. Command:
`PYTHONDONTWRITEBYTECODE=1 python3 wt/cats/dsOMOP/tools/ws0/check_contracts.py wt/cats/dsOMOP wt/cats/dsOMOPClient`.

Both worktrees pass `git diff --check` against their starting heads. All six shared
files (`category_semantics.R`, `category_resolution.R`, `number_semantics.R`,
`date_semantics.R`, `concept_labels.R`, `plan_normalize_v2.R`) remain byte-identical.
No Rd/NAMESPACE changes were needed. Evidence is retained beside this report as
`cats-fix4-*.log`, with exact gzip archives and checksums in the final client commit.

## Deviations and handoff

Only targeted changed-code tests and direct dependents ran, as explicitly requested
and required by COMMON.md's later test-economy rule. The earlier full-suite wording
is superseded; integration/release owns that pass. No independent implementation
approval or full release certification is claimed.

Other jobs must integrate the matching `date_semantics.R` helper in both packages.
The server's endpoint expansion semantics are preserved. Privacy gates, persistent
state, owner configuration and the wire contract are unchanged. Open vocabulary
metadata retains its existing unresolved status.

No Claude CLI, subagents, pushes, PRs, tags or stashes were used. Changes are confined
to the designated worktrees and this job's report/evidence under `design/astra-dev/`.

## Cleanup

`/workspace` had 9.7 GB free before and after the work. CLEANUP.md was read;
no large local copies or unrelated deletions were made. After all test processes
finished and the full logs were copied back, these disposable pod paths were
removed (sizes measured immediately before deletion):

| Deleted pod path | Bytes |
|---|---:|
| `/root/rt/wt/cats/dsOMOP` | 24,756,093 |
| `/root/rt/wt/cats/dsOMOPClient` | 16,320,032 |
| `/root/rt/lib/wt_cats_dsOMOP` | 10 |
| `/root/rt/lib/wt_cats_dsOMOPClient` | 11,778,800 |

Runner/install logs, shared fixtures and dependency libraries remain. Exact
cleanup evidence is in `cats-fix4-cleanup.log`. No other paths were deleted.
R7 has no remaining implementation blocker in the targeted verification;
independent review and integration/release testing remain separate jobs.
