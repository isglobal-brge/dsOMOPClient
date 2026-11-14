# Categorical review 6: shared presentation repair (catsx7)

14 November 2026. Worktrees: `/workspace/dsomop/wt/cats/dsOMOP` and
`/workspace/dsomop/wt/cats/dsOMOPClient`, branch `feat/2.10.5-cats`.

Starting heads: server `d26fb5ed5fd8685b1726e63628fdb0adc37f6566`, client
`b604c76` (the previous R9/R10 repair and its report). Read COMMON.md, the
workstream common rules, cat-extras, A-30/CR-31, reviews 1–6 and the previous
repair report. This job implements review 6's appended coordinator decision,
in addition to retaining its R9/R10 reproductions. Both versions remain 2.10.5.

## Integration

Merged the advanced local integration heads `feat/2.10.3`: server `9e3ad7f`
through merge `6f54db8`, client `f11e988` through merge `d69ea2d`. These were
release-verification documentation updates; there were no conflicts. Final
ancestry checks confirm those integration heads have not moved again.

Final implementation/test heads before the evidence-only commit:

- Server: `c9035a4cf6a6b989610efe27707c40be9ac3bb2e`
- Client: `671cbaf3c3fe36d898d3e75e2ce7454945f95a75`

## Findings and implementation

- **R9 and the R6–R9 recurring presentation class:** `presentation_values.R`
  is identical in both packages and contains execution's category presentation,
  date/number metadata capture and vector type extraction. The prospective
  builder evaluates empty publicly declared prototypes with those functions,
  `.datePresent()` and the shared `number_vector.R` evaluator. Factor classes,
  levels, order, references and contrasts come from execution's resolver and
  factor constructor. Open vocabulary/source domains remain unresolved; no
  patients or vocabulary service are queried to build a codebook.
- **Generated numeric/flag and copy compositions:** shared flag lineage and
  duration eligibility apply to generated columns. Output copies retain their
  canonical numeric or pre-category prototypes and explicit presentation spec,
  so a numeric reset and a categorical copy branch at execution's actual stages.
- **R10:** `.presentationMembershipColumns()` supplies the compiler and the
  prospective inventory with the same declared membership columns, including
  nullable `index_source` after the dates and the compatibility cohort label.
  Existing roster, person-collapse and whole-schema regression tests are retained.
- **Metadata boundary:** release-schema type extraction explicitly excludes
  domains/references/contrasts. Category information continues through the
  existing admitted metadata channel. A regression ensures private factor
  labels cannot appear in the ungated schema descriptor.
- **Additional mismatches exposed by the matrix:** native survival/panel outputs
  retain selected ordinary numeric covariates even without an explicit numeric
  specification. Compiler and codebook share variable-slot ordering; reordering
  preserves frame metadata. Untranslated native reference components carry the
  same proven reference marker needed by codebook reconciliation (the v1 memory
  behavior is preserved).
- **Native multistate identity:** the legacy state machine receives private
  positional surrogate keys; the native adapter restores exact character episode
  keys and person identities afterward. It no longer rounds keys above 2^53 or
  exposes double row keys. The v1 state machine is unchanged. The prototype test
  checks adjacent keys `9007199254740993`/`9007199254740994` and empty results.

NEWS in both packages describes the correction and migration: rebuild codebooks;
existing recipes need no changes. No API, wire schema, version or Rd change.
The client adds `bit64` to Suggests for the shared numeric evaluator's optional
exact-integer branch. Server-only scalar rounding and expression execution stay
outside the shared prototype file; unbound expression schemas stay unresolved.

Finding-to-test mapping (all paths below are under `tests/testthat/`):

| Finding / invariant | Regression evidence |
|---|---|
| R9 structural membership/long dates, generated R8 dates, inactive defaults, public order/recode/reference | `test-category-review-endpoint-types.R`, `test-category-review-schema.R`, the presentation matrix shards |
| R10 declared membership inventory/order/types, roster inheritance and person collapse | `test-presentation-prototypes.R`, `test-category-review-endpoint-types.R` |
| Shared execution/prospective presentation across formats and scopes | `helper-category-equivalence.R` and the `test-category-presentation-*.R` shards; prospective and published books are constructed with DSI calls mocked to error |
| Copies, nested/joined outputs, layout and prior review compositions | `test-category-presentation-equivalence.R`, `test-category-columns.R`, `test-category-layout.R`, `test-category-review-composition.R`, `test-category-review-fixes.R` |
| Metadata gate and lossless multistate keys | `test-presentation-prototypes.R`; compile/publication and matrix checks |

## Targeted verification

**Review-6 repairs are implemented and the targeted verification is complete.**
All package tests use `/workspace/dsomop/tools/rtest.sh` with explicit filters,
`R_LIBS_USER=/root/rt/numeric-deps:/root/rt/labels-deps`, and matching server
installation for client runs. No full suite was invoked.

Completed server verification:

| Source snapshot | Exact files (`tests/testthat/test-*.R`) | Result | Evidence |
|---|---|---|---|
| `38d90e2` | `compile-output`, `date-system-outputs`, `number-system-outputs`, `category-wire` | 2,036 passed; 0 failed/errors/skips | `catsx7-server{,.full}.log` |
| `93e4afc` | `presentation-prototypes`, `number-system-outputs` | 240 passed; 0 failed/errors/skips | `catsx7-server-final{,.full}.log` |
| `c7d30f7` | `compile-output`, `number-system-values`, `number-system-outputs`, `publication`, `publication-v1-reference`, `presentation-prototypes` | 1,438 passed; 1 stale staging assertion failed; 0 errors/skips | `catsx7-server-verified{,.full}.log` |
| `3e82f8f` (only the stale test oracle changed) | `publication-v1-reference` | 170 passed; 0 failed/errors/skips | `catsx7-reference-verified{,.full}.log` |

All six selected server files therefore pass across coherent targeted runs.
There is no second broad rerun for a change confined to the one legacy test.
A subsequent portability edit resolves server-only vocabulary providers lazily
from the shared function, just as the numeric evaluator resolves its execution
providers. That lookup path receives a separate vocabulary regression run;
its empty-prototype branches and presentation semantics are unchanged.

Completed client matrix shard, at server `3e82f8f` and client `38bdd71`:
`test-category-presentation-month.R` passes all **18 combinations / 8,125
assertions**, with zero failures, errors or skips. Evidence:
`catsx7-month-3{,.full}.log`. The shared matrix implementation is in
`helper-category-equivalence.R`.

At the same server `3e82f8f` / client `38bdd71` snapshots, the remaining original
matrix cells and retained review regressions pass **16,874 assertions**, with
zero failures, errors or skips (`catsx7-client-final{,.full}.log`). Exact files:

- `test-category-presentation-day.R`
- `test-category-presentation-relative.R`
- `test-category-presentation-equivalence.R` (canonical numeric/category copies)
- `test-category-codebook.R`
- `test-category-columns.R`
- `test-category-layout.R`
- `test-category-review-composition.R`
- `test-category-review-endpoint-types.R`
- `test-category-review-fixes.R`
- `test-category-review-schema.R`

The combined run also emitted 21 `object dsOexec... not found` cleanup
warnings in refusal-path tests. They are preserved in the full log and were
not suppressed.

The two completed client runs total **24,999 passing assertions**, including all
54 original matrix cells. The later provider-lookup portability edit is tested
separately at the new heads, without repeating the unchanged matrix branches.
A server-side attempt at that supplement (`c9035a4`, files
`test-category-cdm-server.R`, `test-category-contract.R`,
`test-presentation-prototypes.R`) passed 24 assertions but skipped four blocks
because that private library lacked dsOMOPClient or the opt-in fixture. It is
not claimed as vocabulary certification; the matching installed client/server
run of `test-category-vocabulary.R` provides that evidence instead. All skips
are retained in `catsx7-vocabulary-verified{,.full}.log`.

The final supplement at server `c9035a4` / client `671cbaf` passes **6,385
assertions**, with zero failures, errors or skips. Exact files:
`test-category-presentation-logical.R` (18 added matrix cells) and
`test-category-vocabulary.R` (the lazy server-provider lookups, styles,
hierarchies, language candidates and refusal paths). Evidence:
`catsx7-client-supplement{,.full}.log`. It emitted 21 cleanup warnings from
refusal-path vocabulary tests, also retained without suppression.

**All 72 matrix cells pass. The three definitive client runs total 31,384
passing assertions, zero failures/errors/skips.** No assertion was removed to
obtain this result. The month/public-domain fixture and legacy stream test
oracle corrections are described explicitly below.

The property matrix compares prospective declarations, published metadata and
actual DSLite vectors across ten native output formats, all five survival
styles, three date presentations, three numeric presentations, four category
settings (none, nominal, factor flags, logical flags), and recipe/variable scopes. Matrix cells use one synthetic site to
avoid redundant site execution; existing endpoint tests retain three-site memory
and actual Parquet coverage, public order/reference/recode declarations,
inactive vocabulary/binary defaults, nested/joined dependents and roster controls.
Sparse/temporal cells participate only where numeric matrix presentation is
permitted; they cannot carry categorical bin cells. Open long-source inventory
is checked as a prospective subset and its full publication is checked against
actual frames, consistent with review 6's explicit scope.

Preliminary failures are retained in the evidence logs, including fixture
construction/diagnostic errors. They are not counted as passing verification.
`catsx7-matrix-11` and `catsx7-client-verified` were deliberately interrupted
after identifying a failure, to avoid continuing repeated diagnostics. Early
development snapshots were not uniformly pinned; they are excluded from
certification. The definitive tables above identify exact committed snapshots,
and the archived `test-ledger.tsv` inventories every full log and test file.
One preliminary run mixed old loaded server code with newly synced prototype
tests; it is explicitly excluded from certification. The final runs use frozen
source snapshots. The combined client batch needed more time for retained
three-site schema regressions: its 2,400-second watchdog alone was paused and
resumed when the unchanged R process finished. `catsx7-watchdog.log` records
the exact PIDs and R exit status. No R worker was signalled in that extension. The month matrix fixture declares 2019–2021 plus the single public
`2024-12` endpoint month: `level_range` adds public levels and does not clip
observed dates. An intermediate 2020–2025 range was correctly refused by the
fixture’s unchanged 40-level cap; it is recorded as a fixture error. A second fixture correction includes lagged counting-process starts in
December 2019. The final 37-level domain keeps exact domain assertions meaningful within that cap.
The original 54 cells are split into day/month/relative files. A fourth logical-flag
category setting adds 18 cells in its own shard, yielding a 72-cell product
without repeating the passing 54 cells. No warnings or tests were suppressed.

The focused publication run also exposed an old assertion in
`test-publication-v1-reference.R`: it expected integer `concept_id` in a raw
Parquet SQL stream, while `.coerce_integer64(stable=TRUE)` has already specified
character identifier columns since before this task. `R/extraction.R` and
`R/plan.R` are unchanged from starting head `d26fb5e`; the baseline helper and
its commit are recorded in `catsx7-legacy-stream-contract.log`. The test now
asserts the physical descriptor type `character|character` and compares the raw
file against explicit character IDs. Memory still requires integers. The old
CSV result coercion is removed, so both file paths check the real stored values.
This is a correction of the test oracle, not a production behavior change or a
suppressed failure; only this file is rerun after the test-only commit.

The final Python contract checker passes: 3 schemas, 2 examples, 224 probes,
15 acceptance plans, 12 digest vectors, 18 normalized fixtures, 34 numeric
fixtures and 11,043 double-corpus cases (`catsx7-contracts-final.log`). Command:

```sh
PYTHONDONTWRITEBYTECODE=1 python3 wt/cats/dsOMOP/tools/ws0/check_contracts.py wt/cats/dsOMOP wt/cats/dsOMOPClient
```

`catsx7-integrity.log` records versions, integration ancestry, clean source
worktrees and byte identity of the shared category/date/number/normalization
files, including `presentation_values.R` and `number_vector.R`.
`catsx7-portability.log` checks that the shared category function does not bind
server-only vocabulary providers statically in the client.

## Handoff and limits

Only targeted changed-code tests and their direct dependents run. Integration
and release own full-suite verification. Independent review approval is not
claimed. Open source/vocabulary domains and unbound expression schemas remain
unresolved where public declarations do not bind them. This job does not claim
a fresh replay of every historical standalone probe, all C01–C65 variants, the
full GiBleed/Synthea/vendor matrices, or integration/release certification. No subagents, Claude CLI, pushes, PRs, tags or stashes were used.

The workspace root is not a Git repository; the final report and compressed
logs are also archived under the client repository's `tools/categories/` so
all deliverables are committed. Disk initially had 9.6 GB free and ended with 9.5 GB free; CLEANUP.md was
read. No large local package copies or unrelated deletions were made. Only this
job’s disposable pod mirrors/private libraries and named scratch files were
removed after the complete logs were saved; other workers, dependency libraries,
fixtures, runner logs and the coordinator’s launcher transcript were untouched.

### Cleanup evidence

`catsx7-cleanup.log` records the following deleted paths and pre-deletion sizes.

| Path | Bytes |
|---|---:|
| `/root/rt/wt/cats/dsOMOP` | 24764491 |
| `/root/rt/wt/cats/dsOMOPClient` | 16474819 |
| `/root/rt/lib/wt_cats_dsOMOP` | 10 |
| `/root/rt/lib/wt_cats_dsOMOPClient` | 11786876 |
| `/tmp/catsx7-edit.py` | 7977 |
| `/tmp/catsx7-next.py` | 3524 |
| `/tmp/catsx7-order.py` | 2595 |
| `/tmp/catsx7-split-matrix.py` | 1601 |
| `/tmp/catsx7-client-live.log` | 119251 |
| `/tmp/catsx7-matrix6-summary` | 618 |
| `/tmp/catsx7-archive.py` | 2255 |

## Commits

### dsOMOP

- `6f54db8` — Merge branch 'feat/2.10.3' into feat/2.10.5-cats
- `38d90e2` — Share execution presentation and membership schema with public prototypes
- `93e4afc` — Share generated duration eligibility and guard admitted metadata boundary
- `28de7d9` — Retain public reference provenance when concept translation is disabled
- `1c827fd` — Keep scalar numeric execution dependencies outside shared prototypes
- `acd9c81` — Preserve requested numeric covariates and share mixed-presentation column order
- `f60fbe4` — Preserve metadata during covariate ordering and bypass empty reference slots
- `60b9624` — Restore lossless native episode identities after multi-state evaluation
- `c7d30f7` — Scope reference metadata correction to native plans and document type migrations
- `3e82f8f` — Assert the existing stable identifier type in legacy staged reference tests
- `c9035a4` — Resolve execution-only vocabulary providers lazily in shared presentation

### dsOMOPClient

- `d69ea2d` — Merge branch 'feat/2.10.3' into feat/2.10.5-cats
- `277fd65` — Build prospective presentation from execution prototypes and compare format matrix
- `8fa89ed` — Bind generated flags and copy prototypes to shared presentation lineage
- `3d21619` — Encode public infinite numeric bounds in prototype contracts
- `ad62dfe` — Preserve explicit copy specs and diagnose matrix fixture validation
- `66f41d7` — Use explicit scalar broadcast in cross-format equivalence fixtures
- `bee2d62` — Retain canonical numeric prototypes for output-local copies
- `73ee220` — Exercise admissible event and inherited joined slots in the format matrix
- `3fbdf0f` — Apply matrix overrides only to eligible producers and retain diagnostics
- `ce9883b` — Keep prototype numeric evaluation portable and report first matrix mismatch
- `bcb5c60` — Use shared covariate slot ordering in prospective components
- `eb6617a` — Document shared presentation and cross-format corrections in 2.10.5
- `e6a5a79` — Declare the complete synthetic date horizon in equivalence fixtures
- `6497f6a` — Split presentation matrix by date axis for targeted failure diagnosis
- `963d504` — Declare the endpoint month within the fixture factor-domain cap
- `38bdd71` — Include lagged survival starts in the public matrix date domain
- `0ff1ab9` — Keep shared presentation independent of server vocabulary providers
- `671cbaf` — Complete the presentation product with logical flag defaults and overrides

The final client documentation commit archives this report and its evidence; it changes no executable code.
