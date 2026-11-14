# Categorical review 3 repair report

14 November 2026. Worktrees: `/workspace/dsomop/wt/cats/dsOMOP` and
`/workspace/dsomop/wt/cats/dsOMOPClient`, branch `feat/2.10.5-cats`.
Starting heads were the reviewed `e13341b` / `b3f8e42`; both worktrees were clean.
Both package versions remain 2.10.5.

## Finding and repair

R6 is the review's only blocking finding. The client now constructs the public
panel inventory before projection: period measurements, age/calendar partitions,
exposure and outcome measurements, and closed source identities with their
ordered analyses and carry-forward columns. Source-specific analysis restrictions,
interval sources, concept-set references and closed exclusions are handled locally.
Vocabulary-expanded/mapped inventories remain unresolved until admitted metadata;
the builder performs no vocabulary or patient query.

The server's reserved numeric-role enumeration is now the shared
`.numberStructuralColumns()` helper in both copies of `number_semantics.R`.
Prospective projection distinguishes identity keys and reserved event/status roles
from numeric measurements using the component's original types and that same
role enumeration. Panel period measurements and survival follow-up measurements
are therefore selectable, while required keys and event/status columns remain.
Expanded-variable projection retains its existing structural-column behavior.
Joined components apply the same distinction to their inherited schemas.
Reference companions retain their own complete schema.

`test-category-review-schema.R` compares prospective names, published metadata
names and actual frame names/order on all three synthetic sites. It includes the
exact S1 panel and S2 category-mode survival declarations, copy-only selections,
selected generated panel originals, memory and actual Parquet, all five survival
styles, recurrent event/risk-set components, multi-state transition references,
multiple TARs, a joined panel dependent, and a fuller unprojected panel inventory.
DSI aggregate/assign calls are mocked to fail during both codebook constructions,
so the schema checks also verify local construction. Pure inventory checks cover
closed exclusions, inactive analyses and interval analysis ordering.

This is an implementation correction within A-30/CR-31. NEWS in both packages
instructs users to rebuild affected codebooks without changing recipe declarations.
No publication, privacy, persistent-state or configuration rule changes.

## Commits

Server:

- `600c280` — Share structural numeric roles with prospective output schemas.

Client:

- `22986e5` — Build panel inventories and project survival codebooks by typed roles.
- `52f1f05` — Isolate schema probes and handle sources without applicable analyses.
- `33b1b11` — Use populated TARs and interval exposures in schema regressions.

## Targeted verification

All R tests use `/workspace/dsomop/tools/rtest.sh` with nonempty filters and a
1,200-second timeout. Client runs install the matching server worktree with
`RTEST_WITH`. The server-only compiler/numeric tests do not import the client and
run without a sibling installation. All use
`R_LIBS_USER=/root/rt/numeric-deps:/root/rt/labels-deps`. The server run additionally
sets the prescribed PostgreSQL, MariaDB and MySQL hosts and MySQL port 3307.

| Run | Server/client snapshots | Exact test files | Result |
|---|---|---|---|
| Initial regression development | `600c280` / `22986e5` | `test-category-review-schema.R` | 136 passed; 60 frame assertion failures and 2 errors. All prospective-versus-published assertions in S1/S2 passed. Frame assertions read earlier releases because the harness reused output symbols. Multi-TAR execution correctly refused an empty late-event component; the panel exposure declaration incorrectly supplied an event instead of an interval. Tests now use distinct symbols, populated overlapping TARs, and `omop_era()` exposure intervals. Initial failed evidence is retained. |
| Server direct dependents | `600c280` | `test-compile-output.R`, `test-number-system-outputs.R` | **1,085 passed; 0 failures/errors/skips; no testthat warnings.** |
| Corrected R6 regressions | `600c280` / `33b1b11` | `test-category-review-schema.R` | **235 passed; 0 failures/errors/skips; no testthat warnings.** |
| Client direct dependents | `600c280` / `33b1b11` | `test-category-review-composition.R`, `test-category-review-fixes.R`, `test-category-codebook.R`, `test-category-columns.R`, `test-category-layout.R` | **479 passed; 0 failures/errors/skips.** |

Exact filters: `^category-review-schema$`,
`^(compile-output|number-system-outputs)$`, and
`^category-(review-composition|review-fixes|codebook|columns|layout)$`.
The final successful targeted runs total **1,799 assertions**. The client
dependent run retains 21 rollback-cleanup warnings about missing execution-owned
temporary symbols in expected-refusal tests. Initial development has three such
warnings. No warnings were suppressed and no tests were disabled.

The final Python contract check at `600c280` / `33b1b11` passed 3 schemas,
2 examples, 224 probes, 15 acceptance plans, 12 digest vectors, 18 normalized
fixtures, 34 number fixtures and 11,043 double-corpus cases. Command:
`PYTHONDONTWRITEBYTECODE=1 python3 wt/cats/dsOMOP/tools/ws0/check_contracts.py wt/cats/dsOMOP wt/cats/dsOMOPClient`.
The six shared files `category_semantics.R`, `category_resolution.R`,
`number_semantics.R`, `date_semantics.R`, `concept_labels.R` and
`plan_normalize_v2.R` remain byte-identical.

Both worktrees pass `git diff --check` against the starting heads, and were clean
at their tested implementation heads. No Rd/NAMESPACE changes were needed.

Evidence is retained alongside this report as `cats-fix3-{first,schema,server,client}.log`
and corresponding `.full.log` files, plus `cats-fix3-contracts.log`,
`cats-fix3-integrity.log` and `cats-fix3-cleanup.log`. The final documentation
commit archives this report under client `tools/categories/cats-fix3.report.md`
and exact gzip-compressed evidence under `tools/categories/cats-fix3-evidence/`,
with original and archive SHA-256 checksums. Raw log whitespace is preserved.

## Scope and handoff

The explicit task instruction and later test-economy rule in COMMON.md govern:
only changed-code tests and direct dependents run. Integration/release owns the
full-suite pass. No new design amendment or independent review is claimed.
No Claude CLI, subagents, pushes, PRs, tags or stashes are used. Only the designated
worktrees and this job's report/evidence under `design/astra-dev/` are changed.

Other jobs must retain identical copies of `number_semantics.R`, including the
shared structural-role helper, when integrating these commits. Existing
vocabulary-dependent display names and open schemas retain their unresolved
status; this repair does not claim a new vocabulary-resolution mechanism.


## Cleanup

`/workspace` had 9.7 GB free before and after the work. CLEANUP.md was read;
no large local copies were made and no unrelated files were deleted. Once all
workers finished and their full evidence logs were copied back, only this job's
disposable pod mirrors and private libraries were removed:

| Deleted pod path | Bytes before deletion |
|---|---:|
| `/root/rt/wt/cats/dsOMOP` | 24,751,495 |
| `/root/rt/wt/cats/dsOMOPClient` | 16,282,202 |
| `/root/rt/lib/wt_cats_dsOMOP` | 10 |
| `/root/rt/lib/wt_cats_dsOMOPClient` | 11,777,845 |

Runner/install logs, dependency libraries and shared fixtures remain. No other
paths were deleted. There are no unresolved implementation blockers from review 3;
independent verification and integration/release testing remain separate jobs.
