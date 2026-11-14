# Categorical review 5 repair report

14 November 2026. Worktrees: `/workspace/dsomop/wt/cats/dsOMOP` and
`/workspace/dsomop/wt/cats/dsOMOPClient`, branch `feat/2.10.5-cats`.
Starting heads were the clean reviewed `f5c88ce` / `416a945`.
Both package versions remain 2.10.5.

## Finding mapped to repair and tests

**R8 (the sole blocking finding): corrected and verified.**
Generated date endpoint declarations now receive the effective recipe category
defaults. The shared prospective column helper applies factor ordering and recode
types after producer typing, including when no variable node exists. Generated
dates deactivate vocabulary and binary fields, matching publication. The existing
public-domain resolver then applies category identity, labels, order, reference
and contrasts. Open period domains remain unresolved without a public declaration.

The recode-domain loop now uses a separate target variable so adding public target
records cannot overwrite the output column's stable key/name. This is necessary
to retain names/order when the newly enabled generated-date recode path runs.

`tests/testthat/test-category-review-endpoint-types.R` covers:

- The exact review D1 panel recipe with `ordered=FALSE` and its no-category-default
  control, in memory and staged Parquet, at all three synthetic sites.
- Prospective versus published names/order, endpoint R type and category ordering/
  contrast declarations; actual factor class, ordering and levels at every site.
- Both codebook constructions with DSI aggregate/assign functions mocked to error;
  every staged component checked to be a Parquet `FlowerDatasetDescriptor`.
- Direct nested recurrent-event/risk-set components with multiple TARs, and joined
  panel outputs, in both transports with and without unordered defaults.
- Public month domains with explicit reverse order/reference, nominal recodes and
  an ordered override: prospective/published keys, labels, reference and contrasts
  agree, and actual contrast matrices match those declarations.

Date-only published metadata omits raw ordered/contrast fields, as documented by
review 5; controls compare the declared R type and actual ordered-factor class.
The client fix preserves the repaired endpoint expansion and projection order.
NEWS in both packages classifies the correction and tells users to rebuild their
codebooks without changing recipes. This implements A-30/CR-31's approved contract;
no new amendment is needed.

## Commits

- Server `1a10842279cbcefddbb8122e4cdb24fe9543ccb8` — Document generated date
  category codebook correction (NEWS only).
- Client `4bfac467c173005cfe1c579b9eae25e21dae0533` — Apply category defaults to
  prospective generated date factors, with R8 publication regressions.
- Client `b6b7a951e61ac7a813e7562d4ec607b81d0cbd25` — Inspect staged endpoint
  contrasts before test frame concatenation (test-only correction).
- The final client documentation commit archives this report at
  `tools/categories/cats-fix5.report.md` and the exact compressed logs/checksums at
  `tools/categories/cats-fix5-evidence/`.

## Targeted verification

The initial new-file run tested server `1a10842` / client `4bfac46` through
`rtest.sh` with filter `^category-review-endpoint-types$` and a 1,800-second timeout:
**1,518 passed; 12 failed; 0 errors/skips**. The exact R8 reproductions, controls,
nested and joined cases passed. The 12 failures were solely actual contrast-matrix
checks on staged frames after the test helper's `rbind.data.frame()` removed
contrast attributes. A minimal probe confirms that even concatenating one factor
frame strips those attributes. `b6b7a95` corrects this test artifact by inspecting
each publication-reader chunk directly; no product change or assertion removal.
Initial summary/full logs and the probe log are retained.

**Final combined verification: 3,192 passed; 0 failures/errors/skips.**
It tested server `1a10842` / client `b6b7a95` with filter
`^category-(review-endpoint-types|review-schema|review-composition|review-fixes|codebook|columns|layout)$`
and a 2,400-second timeout. Exact files:

- `test-category-review-endpoint-types.R`
- `test-category-review-schema.R`
- `test-category-review-composition.R`
- `test-category-review-fixes.R`
- `test-category-codebook.R`
- `test-category-columns.R`
- `test-category-layout.R`

The final run retains the same 21 rollback-cleanup warnings documented by review 5
about missing execution-owned temporary symbols in expected-refusal tests. The new
R8 file has no testthat warnings. No warnings were suppressed and no tests were
disabled. Final evidence: `cats-fix5-client.log` and `cats-fix5-client.full.log`.
Initial evidence: `cats-fix5-endpoint-types.log`,
`cats-fix5-endpoint-types.full.log`, and `cats-fix5-test-helper.log`.

Both R runs use `/workspace/dsomop/tools/rtest.sh`, install the matching server via
`RTEST_WITH`, and set `R_LIBS_USER=/root/rt/numeric-deps:/root/rt/labels-deps`.

The Python contract checker passes: 3 schemas, 2 examples, 224 probes, 15 acceptance
plans, 12 digest vectors, 18 normalized fixtures, 34 number fixtures and 11,043
double-corpus cases. Command:
`PYTHONDONTWRITEBYTECODE=1 python3 wt/cats/dsOMOP/tools/ws0/check_contracts.py wt/cats/dsOMOP wt/cats/dsOMOPClient`.
It ran on `1a10842` / `4bfac46`; the subsequent client change only corrects test
inspection. Evidence: `cats-fix5-contracts.log`.

## Deviations and handoff

Only targeted changed-code tests and direct dependents ran, following the user's
instruction and COMMON.md's later test-economy rule. Full suites belong to the
integration/release job. The server has no implementation changes in this repair;
matching server execution is exercised through the client publication regressions.
No independent implementation approval or full release certification is claimed.

Both worktrees pass `git diff --check` against their reviewed starting heads.
`cats-fix5-integrity.log` records the exact tested commits, clean status, matching
versions and byte identity of `category_semantics.R`, `category_resolution.R`,
`number_semantics.R`, `date_semantics.R`, `concept_labels.R` and
`plan_normalize_v2.R`. No wire, privacy-gate, persistent-state, database configuration, version or Rd/NAMESPACE
changes are required. Other jobs should integrate the client codebook repair;
server publication semantics are preserved.

No Claude CLI, subagents, pushes, PRs, tags or stashes were used. Changes are confined
to the designated worktrees and this report/evidence under `design/astra-dev/`.
The workspace root is not a Git repository, so the report and logs are also
archived in the client repository to commit all deliverables.

## Disk and cleanup

The workspace had 9.6 GB free at both checks. CLEANUP.md was read; no large local
copies or unrelated deletions were made. After the tests finished and the full logs
were copied back, only this job's disposable pod mirrors and private installation
library were removed. Runner/install logs, shared fixtures and dependency libraries
remain. Exact cleanup evidence is in `cats-fix5-cleanup.log`:

| Deleted pod path | Bytes before deletion |
|---|---:|
| `/root/rt/wt/cats/dsOMOP` | 24,756,426 |
| `/root/rt/wt/cats/dsOMOPClient` | 16,363,507 |
| `/root/rt/lib/wt_cats_dsOMOPClient` | 11,779,205 |
