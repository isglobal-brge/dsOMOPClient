# Library review 2 fix report

Date: 2026-11-14. Branch: `feat/2.10.5-library` in both assigned worktrees.
Status: **R3 addressed; targeted verification complete.** R1 and R2 were already
accepted by review 2 and required no further changes.

## Commits

Server:
- `a9b9424fd2d3593aba18e8e69e21d8e8464b3b69`: row-count-sized death availability,
  focused native regressions and NEWS migration note.
- `2beed9c63a10573e7f6e855edd1598ad1b01a82e`: omit absent variables from native
  regression wire fixtures. This is the final tested server revision.

Client:
- `69700920ee0d7e2878f2c6ddec48669eb80c0568`: supported-plus-empty comparator
  regressions and NEWS migration note.
- `4cf201f25cd33bb8eb186e01c8df6ab63bfa8c02`: compare transported column values
  rather than memory/staged frame attributes.
- `c4b82ba6cc83df0dcd18386c50bef152b3f84cc8`: restrict retained-frame reads to the
  supported site. This is the final tested client revision.
- A final documentation/evidence commit contains this report, the acceptance
  matrix addition and the four logs plus their checksum manifest; it changes
  no tested code. Its hash is given in the completion response.

## Finding, fix and tests

| Finding | Correction | Verification |
|---|---|---|
| R3: empty native death rosters throw before the support gate, preventing explicit comparator-site skipping | `R/compile_reduce.R` now assigns `rep(!is.null(role), nrow(rows))` to `available`. Zero rows remain zero rows; nonempty behavior and memoization are preserved. No exception is reclassified as a support refusal. | Server `test-compile-death-empty.R`: typed zero-row death frames for available/unavailable sources and repeat memo reads; death-censored survival outputs from an empty source roster or eligibility-filtered roster reach `insufficient_support` in memory and staged modes, publishing nothing and removing staging artifacts. Client `test-recipe-library-empty-comparator.R`: exact default T15 recipe on two nfilter=5 sites, with only site2's drugs removed or its observation history shortened so every candidate fails the existing 365-day eligibility criterion. Both modes verify default `insufficient_support`, exact symbol and file rollback, explicit skip with no site2 publication, both site1 roots retained, 24 supported persons, independent expected rows/schemas and matching memory/staged values. |

The source-empty client case deletes only site2's drug records. The eligibility
case retains all 72 drug records and sets observation-period starts to
2020-01-01; every candidate has less than the default required 365 prior days.
Site1 and all recipe defaults, eligibility rules, publication roots and support
thresholds stay unchanged. Existing missing-table and candidate-cap boundary
tests still refuse and roll back rather than skipping technical failures.

The production diff is one server line. Both NEWS files describe the correction
and migration: update the matching server and re-run affected recipes. The
acceptance matrix links the exact new two-site tests.

## Targeted verification

All runs used the prescribed pod runner and matching sibling via `RTEST_WITH`.
No full suites were run. Exact commands, revisions, file lists, environments,
counts, log paths and SHA-256 values are in
`inst/recipe-library/evidence/library-fix2-tests.json`. Complete logs are beside
that manifest and retained under `/root/rt/wt/library/library-fix2-*.log`.

| Run | Revisions (server / client) | Exact files | Result |
|---|---|---|---|
| Server dependencies and initial regression | `a9b9424` / `6970092` | `test-compile-reduce.R`, `test-compile-output.R`, `test-publication-compiled.R`, `test-compile-death-empty.R` | 1,245 passed, 0 assertion failures, 2 fixture errors, 0 skipped. All three existing dependency files passed. Both new cases initially encoded absent variables as an array and stopped at schema validation; fixed in `2beed9c`. |
| Final native regression | `2beed9c` / `4cf201f` | `test-compile-death-empty.R` | **40 passed; 0 failures, errors, skips or warnings.** |
| Client dependencies and initial regression | `2beed9c` / `4cf201f` | `test-recipe-library-boundaries.R`, `test-v2-execution.R`, `test-recipe-library-empty-comparator.R` | 85 passed, 0 assertion failures, 2 test-helper errors, 0 skipped. Both existing dependency files passed. New cases verified default refusal/rollback and successful memory skip, then attempted to fetch absent frames at the skipped site; fixed in `c4b82ba`. Nine existing cleanup warnings. |
| Final public two-site regression | `2beed9c` / `c4b82ba` | `test-recipe-library-empty-comparator.R` | **141 passed; 0 failures, errors or skips.** Eight existing cleanup warnings (`dsOexec` object not found) on expected default-refusal paths. Symbol and staged-file rollback assertions all passed. |

The client runs had only the acceptance-matrix documentation addition outside
the named commits. Fixture recording was disabled. The final focused tests
cover two empty causes × two modes × default/skip publication policies. The
passing existing dependency files were not repeated after test-only fixture
repairs, following COMMON.md's test-economy rule. No new vendor or shared-demo
execution is claimed.

## Scope, deviations and handoff

- R3's required correction is complete with no scope deviation. The native fix
  is in this server branch, so integration must take it together with the client
  regression changes; a client-only update cannot fix the defect.
- Full suites and the paired 2.10.5 version bump remain integration/release work.
  Both packages still report 2.10.4, as in the reviewed pair.
- No template changes, wire/API/schema changes, arbitrary exception-to-skip
  conversion, owner configuration, persistent state or privacy-policy changes.
- No claude CLI, push, PR, tag, stash, other worktree edit or shared-data change.
  Existing inherited compiler work was retained; its relevant reducer/output
  and publication behavior was checked by the targeted dependency tests.
- Disk was about 9.9 GB free. Following CLEANUP.md, removed only the permitted
  `/home/dev/.cache/pip` download cache (923,541 bytes). This did not recover the
  requested 15 GB reserve; no safe large disposable local cache was identified.
  No full test run or large local package copy was made. Broader shared-disk
  cleanup remains a coordinator task; no other worktree was cleaned.
- After preserving evidence, removed this job's pod mirrors and private test
  libraries: `/root/rt/wt/library/dsOMOP` (24,188,391 bytes),
  `/root/rt/wt/library/dsOMOPClient` (18,970,839 bytes),
  `/root/rt/lib/wt_library_dsOMOP` (5,484,061 bytes), and
  `/root/rt/lib/wt_library_dsOMOPClient` (11,167,696 bytes).

Requested external report:
`/workspace/dsomop/design/astra-dev/library-fix2.report.md`.
The identical committed copy is `inst/recipe-library/library-fix2.report.md`.
