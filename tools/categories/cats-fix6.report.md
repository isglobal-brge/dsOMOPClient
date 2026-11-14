# Categorical review 6 repair report

14 November 2026. Worktrees: `/workspace/dsomop/wt/cats/dsOMOP` and
`/workspace/dsomop/wt/cats/dsOMOPClient`, branch `feat/2.10.5-cats`.
Starting heads were clean server `1a10842279cbcefddbb8122e4cdb24fe9543ccb8`
and client `1d06e621c0dc999076d4d18ea8c282bb6fb7690e`.
Both package versions remain 2.10.5.

## Findings mapped to fixes and tests

**R9 — existing output date category defaults.** The initial structural-column
path now supplies recipe category defaults to the same prospective column helper
used by generated endpoints. Without a variable node, only concept/date factors
consume categorical presentation. Nonconcept dates deactivate vocabulary fields;
nonflag producers deactivate binary defaults. Keys and ordinary measures retain
no categorical declaration. Effective order, recode, reference, levels and
contrasts then follow the existing public-domain resolver.

**R10 — membership index provenance.** Membership's declared inventory now derives
from the output and its native population kind. Index, criteria, set-operation and
matched rosters all carry nullable `index_source`; it is declared as character
after the cohort dates, including unindexed person rosters and inherited rosters.
The existing identity selection handles person collapse. Whole-schema assertions
also exposed an adjacent pre-existing declaration mismatch: native BIGINT row keys
arrive through `.reduceFetch()` / `.reduceTypeFrame()` as lossless decimal text,
not R integer64. Prospective `row_id` and `cohort_row_id` now correctly declare
character across native output families; person pseudonyms were already character.
An explicitly declared compatibility cohort label retains its position before the dates. No patient or
source-schema lookup is used.

`tests/testthat/test-category-review-endpoint-types.R` retains the generated
panel, nested recurrent survival/risk-set and joined regressions, and adds:

- The exact membership and long recipes from review 6, each with unordered
  defaults and no-category-default controls, in memory and staged Parquet.
- Prospective/published date R types, ordering and contrast declarations; actual
  factor classes, ordering and levels at every one of three synthetic sites.
- Whole membership schema names, order and R types compared prospectively,
  against published metadata, and against every actual frame/chunk.
- Public month order/reference, nominal recodes and an ordered override;
  prospective/published keys, labels, reference and contrasts, and actual
  contrast matrices in both transports for both membership and long outputs.
  Long concept IDs remain numeric in these period-specific tests, keeping the
  declarations applicable to the date domain rather than unrelated concepts.
- Inactive vocabulary style/language/hierarchy (including `multiple="all"`)
  and binary defaults on those date producers, in both transports.
- Membership from event-index, inherited, cohort-index and union rosters at
  episode and person grain, plus unindexed rosters at person grain, in both
  transports.

Both codebook builders run with DSI aggregate and assign functions mocked to
error. Staged descriptors must identify Parquet; chunks are inspected directly
so concatenation cannot discard factor contrasts. Date-only metadata omits raw
ordered/contrast fields, so controls compare R types and actual factor ordering.
Long source-specific inventory remains open, as explicitly excluded from review
6's blocking findings; all actual long names are still checked against published
metadata. Complete long-schema certification is not claimed.

NEWS in both packages documents the corrections and migration: rebuild affected
codebooks without changing recipes. This implements the approved A-30/CR-31
contract and requires no amendment.

## Commits

- Server `d26fb5ed5fd8685b1726e63628fdb0adc37f6566` — Document membership and long codebook corrections (NEWS only).
- Client `ab74dc3fbe057140e0bfefa3021713e1a1d71509` — Apply category defaults and roster provenance to prospective
  output schemas, with publication regressions.
- Client `24fe524c691163578bdcd7f833a109db2807e0cd` — Declare native row keys using their lossless published type;
  correct the additional unindexed-roster test's grain and explicit collapse.
- The final client documentation commit archives this report and compressed
  evidence/checksums under `tools/categories/cats-fix6*`.

## Targeted verification

Initial targeted run on server `d26fb5e` / client `ab74dc3`, filter
`^category-review-endpoint-types$`, timeout 1,800 seconds:
**2,912 passed; 48 failed; 1 error; 0 skipped.** All 48 assertion failures compared
old prospective integer64 row-key types with actual/published character keys;
the date semantics and index-provenance names passed. The error was the extra
roster fixture requesting episode grain from an unindexed person population.
A construction-only pod probe records that explicit validation error.
`24fe524` fixes the key declarations and uses the required person grain for that
fixture, with explicit first-episode collapse for indexed person outputs.
Evidence: `cats-fix6-endpoint-types.log`, `cats-fix6-endpoint-types.full.log` and
`cats-fix6-roster-probe.log`. No assertions or warnings were suppressed.

Final combined targeted run on server `d26fb5ed5fd8685b1726e63628fdb0adc37f6566`
and client `24fe524c691163578bdcd7f833a109db2807e0cd`, with a 2,400-second timeout and filter:

```text
^category-(review-endpoint-types|review-schema|review-composition|review-fixes|codebook|columns|layout)$
```

Exact test files:

- `test-category-review-endpoint-types.R`
- `test-category-review-schema.R`
- `test-category-review-composition.R`
- `test-category-review-fixes.R`
- `test-category-codebook.R`
- `test-category-columns.R`
- `test-category-layout.R`

**Final result: 5,376 passed; 0 failures, errors or skips.** Both blocking
findings are corrected and verified, including the additional row-key declaration
correction required by whole-schema type assertions. Evidence:
`cats-fix6-client.log` and `cats-fix6-client.full.log`.

The same 21 rollback-cleanup warnings documented by review 6 remain, about missing
execution-owned temporary symbols in expected-refusal tests. The expanded endpoint
file has no testthat warnings. No warnings, tests or assertions were suppressed.

Both R runs use `/workspace/dsomop/tools/rtest.sh`, installing the matching server
through `RTEST_WITH`, with
`RTEST_ENV='R_LIBS_USER=/root/rt/numeric-deps:/root/rt/labels-deps'`.
The small construction-only probes run on the same pod/library/source; the
corrected probe validates both grain variants without execution.

The Python contract checker passes on both client implementation commits:
3 schemas, 2 examples, 224 probes, 15 acceptance plans, 12 digest vectors,
18 normalized fixtures, 34 number fixtures and 11,043 double-corpus cases.
Command:
`PYTHONDONTWRITEBYTECODE=1 python3 wt/cats/dsOMOP/tools/ws0/check_contracts.py wt/cats/dsOMOP wt/cats/dsOMOPClient`.
Evidence: `cats-fix6-contracts.log` and `cats-fix6-contracts-final.log`.

Both worktrees pass `git diff --check` against the reviewed starting commits.
`cats-fix6-integrity-final.log` records the exact tested commits, clean status,
matching 2.10.5 versions, and byte identity of `category_semantics.R`,
`category_resolution.R`, `number_semantics.R`, `date_semantics.R`,
`concept_labels.R` and `plan_normalize_v2.R`.

## Deviations and handoff

Only targeted changed-code tests and direct dependents ran, following the user's
instruction and COMMON.md's later test-economy rule. Full suites belong to the
integration/release job. The server has no implementation changes; its matching
publication code is exercised by the client integration tests. No independent
review approval or full release certification is claimed.

Other jobs should integrate the client codebook repair. No wire, privacy-gate,
persistent-state, database configuration, version, Rd or NAMESPACE changes are
needed. No Claude CLI, subagents, pushes, PRs, tags or stashes were used. Changes
are confined to the designated worktrees and report/evidence in
`design/astra-dev/`. The workspace root is not a Git repository, so this report
and evidence are also archived in the client repository to commit deliverables.

## Disk and cleanup

The workspace had 9.6 GB free at the initial and final integrity checks.
CLEANUP.md was read; no large local copies or unrelated deletions were made.
After the completed test logs were copied back, only this job's disposable pod
mirrors and private installation library were deleted. Runner/install logs,
shared fixtures and dependency libraries remain. Exact cleanup evidence is in
`cats-fix6-cleanup.log`:

| Deleted pod path | Bytes before deletion |
|---|---:|
| `/root/rt/wt/cats/dsOMOP` | 24,756,703 |
| `/root/rt/wt/cats/dsOMOPClient` | 16,400,531 |
| `/root/rt/lib/wt_cats_dsOMOPClient` | 11,779,502 |
