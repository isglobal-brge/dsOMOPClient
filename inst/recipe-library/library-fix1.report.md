# Library review 1 fixes

Date: 2026-11-14. Branch: `feat/2.10.5-library`.
Status: R1 and R2 addressed; targeted verification complete.

## Commits and scope

- Client `172f924`: enforce explicit scale/unit policies for direct and resolved
  numeric library event bindings; document migration and add regressions.
- Client `021d403da260e3dd1a16f0cecbd38822ed6a2296`: complete the per-entry
  acceptance matrix, independent controls and artifact bundles.
- Client `0763685`: compute vendor-fixture checksums before schema loading,
  fixing the PostgreSQL test harness namespace issue.
- Client `57d68de`: exercise the T13 README ratio mutation example, including
  zero/missing denominators, keys and two-site rollback.
- Final evidence/report commit follows the tested implementation commits.
- Server remains unchanged and clean at
  `18b096ffa72feb27dd7f99fcc2cf1ab43a10dae9`.

Repository changes were confined to the assigned client worktree; this
requested external report was also written. No claude CLI, full suites, pushes, PRs, tags, stash, shared-demo data
changes or server configuration changes were used. Both versions remain
2.10.4 for the integration owner's paired 2.10.5 bump.

## Blocking findings

| Review finding | Fix | Tests/evidence |
|---|---|---|
| R1: missing measurement unit/scale validation | A shared client check accepts native `omop_unit()` restriction/conversion policies or predicates proving a single `unit_concept_id`. AND restrictions intersect; OR requires every branch to restrict the same unit. Unqualified or multi-unit numeric events refuse with a migration message. Both `panel.measurements` and `socioeconomic.numeric_observation` apply the rule to direct bindings and the resolved compiled event. No row-derived inference or server settings. | `test-recipe-library-units.R`: initial/later binding, JSON/YAML import, deferred references, prefixed composition, explicit unitless observation scale, mixed-unit public refusal with unchanged symbols, conversion and restriction execution controls. T04/T20 variant artifacts cover affine conversion, unknown units, missing values and carry boundaries. |
| R2: incomplete per-entry boundary/variant acceptance matrix | Added 109 variant bundles, alongside the 24 default bundles: 133 complete cases, each with bindings, advanced recipe, independent literal control, wire plan, normalized plan, semantic payload/hash, hand-calculated expected rows, full released schema and manifest. Added public memory/staged execution and refusal/rollback checks, schema/order checks, binding/serialization/control/digest checks, and cross-dialect matching membership checks. The T14 control now uses an explicit A-23 lowering rather than the helper under test. | `test-recipe-library-variants.R`, plus the exact existing library tests mapped in `inst/recipe-library/acceptance-matrix.md`. Every T01–T24 row maps to named case directories and its arithmetic. Expected values are authored in the fixture helper, never captured from executor rows. |

The matrix explicitly includes the review's highlighted omissions:

- T13: observed union numerator/denominator 4/4; pre-window carry; the A-20
  cap-binding 200/10/10 example; unavailable and kept-uncovered values.
- T15: same-day arm A priority; own-arm restart versus opposite-arm switch;
  courses failing to contain day-1 entry; prior starts/outcomes and censor ties.
- T20: two declared concepts with records; whole-site empty and unavailable
  sources with identical dense schemas; lookback/expiry equality and outside
  boundaries; no future carry.

## Targeted runs

On client `021d403da260e3dd1a16f0cecbd38822ed6a2296`, with matching server
`18b096ffa72feb27dd7f99fcc2cf1ab43a10dae9`, the final seven-file targeted run
passed **7,237 assertions; 0 failures, errors or skips**. Recording was disabled.
It covered all 24 defaults and 109 variants on SQLite, including memory/staged
publication or expected refusal/rollback. There were 31 existing cleanup
warnings (`dsOexec` object not found) on expected refusal paths.

Exact files:

- `test-recipe-library.R`
- `test-recipe-library-variants.R`
- `test-recipe-library-units.R`
- `test-recipe-library-boundaries.R`
- `test-recipe-library-execution.R`
- `test-v2-merge.R`
- `test-v2-merge-deferred-base.R`

Command:

```sh
RTEST_WITH=/workspace/dsomop/wt/library/dsOMOP \
RTEST_ENV='DSOMOP_LIBRARY_BACKENDS=sqlite' \
  /workspace/dsomop/tools/rtest.sh /workspace/dsomop/wt/library/dsOMOPClient \
  '^recipe-library$|^recipe-library-variants$|^recipe-library-units$|^recipe-library-boundaries$|^recipe-library-execution$|^v2-merge$|^v2-merge-deferred-base$' 2400
```

The vendor-boundary run on client `076368501b8e3aa395d43bd19441568a2d7fa8fc`
and the same server passed **3,270 assertions; 0 failures, errors or skips**.
It ran only `test-recipe-library-variants.R`: 16 selected cases on SQLite,
PostgreSQL, MariaDB and MySQL (64 case/backend checks), in memory and staged
modes. This includes stockpiling, unavailable sources, comparator censors,
two-concept dense schemas, calendar/SCCS boundaries, matching membership
reproducibility, candidate-cap refusal and distinct-person support. There
were 16 existing cleanup warnings on expected refusals.
The exact selector and environment are in `evidence/library-fix1-tests.json`.

The only implementation change after the seven-file run is test-harness
checksum collection before vendor copying (`0763685`); library production
code and frozen plans/results are unchanged. The later ratio test adds no
production change. On client `57d68dea2b008dcc668ac25e3ddf9941e060de5b`,
`test-recipe-library-ratios.R` passed **38 assertions; 0 failures, errors, skips
or warnings**. It executes the existing T13 README mutation example on two
sites: PDC 1, MPR 2.5, zero/NA denominators, unchanged keys, and rollback of a
refused in-place key edit. Command filter: `^recipe-library-ratios$`, timeout
600, with the same server sibling installed.

Independent Python validation on `0763685` also passed for all **133 bundles**:
wire-schema validation, exact equality with the shared Python reference
normalizer, normalisation idempotence, and independent SHA-256 verification
of the canonical semantic payloads. Evidence: `library-fix1-python-reference.log`.


Earlier focused results:

- R1 plus direct merge dependencies on the source committed as `172f924`:
  **1,140 passed, 0 failed/errors/skipped**. Exact files:
  `test-recipe-library-units.R`, `test-recipe-library.R`, `test-v2-merge.R`,
  `test-v2-merge-deferred-base.R`.
- Final recording/control repair subset on the source subsequently committed
  as `021d403`: **647 passed, 0 failed/errors/skipped**. Exact files:
  `test-recipe-library-variants.R` (new-user, recurrent-survival and distinct-
  person-support cases), `test-recipe-library-units.R`, and
  `test-recipe-library-boundaries.R`.

Recording iterations corrected fixture/harness assumptions before freezing:
primitive constructor rendering, fixture hashing of missing data, missing
source creation before connection, separate concept-labelled measurement
columns, observation-gap partitioning, raw-event washout and recurrent
sentinels, and multi-component publication inventories. One setup command
with an unquoted case-selection pipe failed before tests and was rerun with
proper quoting. The first vendor attempt also exposed an unqualified
PostgreSQL checksum query in the test harness, fixed in `0763685` before
rerunning. None of these incomplete attempts is counted as passing evidence.

## Interpretation and handoff

- Full suites remain integration/release work under COMMON.md's later explicit
  targeted-test rule. No full-suite completion is claimed.
- These are corrections to unreleased templates; no released 1.0.0 artifact was
  silently replaced. NEWS documents newly refused unqualified numeric inputs
  and the native restriction/conversion migration routes.
- Kept-uncovered adherence makes observation-dependent values NA. Raw clipped
  supply remains defined by the index/window (10 days in that case), consistent
  with the shared semantics; it is not an observation-dependent denominator.
- Dynamic measurement presentation labels columns while the native column
  contract retains wire identifiers. The schema fixtures pin both names and
  assert the explicit synthetic-vocabulary mapping, including column order.
- Additional native issue observed during fixture development: an entirely
  ineligible active-comparator roster raised `replacement has 1 row, data has
  0` rather than an ordinary support refusal. The required arm-washout cases
  verify exclusions with an adequately supported unaffected roster. This
  separate empty-roster/native-output defect needs integration follow-up; it
  was not reclassified as a support skip or hidden by changing policy.
- No native wire, normaliser, digest, publication, privacy, or execution API
  change was needed for R1/R2. No new persistent state was introduced.

The committed copy is `inst/recipe-library/library-fix1.report.md`.
`inst/recipe-library/evidence/library-fix1-tests.json` records exact revisions,
files, selectors, environments, results and log SHA-256s; compact evidence logs
and independent-reference results are beside it. Each case manifest links to
that record. Full logs remain on the pod at the recorded paths.

Own pod package mirrors/private test library and local scratch files were
removed after evidence capture. The assigned local worktrees remain intact.
The final evidence/report commit changes documentation and manifests only.
