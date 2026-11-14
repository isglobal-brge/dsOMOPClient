# Reproducing the book

Use the locally released 2.10.4 pair. From the client package root, run
`tools/book/run-pod.sh`. It installs only the book pair in its private pod library
and runs `precompute.R`. If absent, it first loads the two pinned public practice
databases with `load-practice.R`. The pod also needs dsBase/dsBaseClient (the release test
library `/root/rt/labels-deps`), DSLite, DBI, RSQLite, resourcer, knitr and withr.

The source is `vignettes/book-*.Rmd.orig`. Knitr executes it against the
synthetic SQLite fixture and writes `.Rmd` containing ordinary fenced code and
recorded output. Thus `pkgdown::build_site()` needs no live database. Each chapter
executes its visible fresh-session setup; the precompute runner never creates
`conns` or selects a dataset for the reader. The downloadable helper ZIP supports
readers without a source checkout. `connection.R` is the sole public-practice
driver/project/resource configuration. Assertions in `verify.R` inspect synthetic server memory only;
none of those row-reading calls is an analyst lesson.

The fixture extends the released `helper-dates-va.R`: 24 synthetic people,
six copies of four patterns, with inpatient/outpatient concepts and a second
fever answer. Book 2 adds separate measurements, prescriptions and results cohort 1
without changing the original observation/visit/death records. It retains threshold 5, ordinary factor gates and pseudonyms.
The second optional site differs only in public vocabulary labels/version.
It is not Eunomia GiBleed; the optional Opal deployment is described separately.

DSLite 1.5 has a quoted-string lexer defect exposed by dsBase calls. The fixture
uses the same scoped tokenizer repair as the released concept-label analysis
tests. It does not mock ds.table, results, allowlists or disclosure checks. The
repair is restored when the fixture scope ends and is unnecessary for Opal.

The bslib theme uses the [OHDSI web style guide](https://www.ohdsi.org/web/wiki/development%3Astyle_guide)
navy `#003142`, orange `#f19119`, blue `#337ab7` and system font stack.
Body text is intentionally larger than the old application guide for book reading.

After precomputing, run `Rscript tools/book/build-site.R` on the pod. This invokes
`pkgdown::build_site()` with reference examples disabled; the book code has already
executed and the build itself never needs a server. Retrieve `docs/` and inspect
it with `python3 tools/book/audit-content.py` and
`PLAYWRIGHT_BROWSERS_PATH=/home/dev/.cache/ms-playwright NODE_PATH=/usr/local/lib/node_modules node tools/book/audit-site.js` (the latter
needs Playwright Chromium). Copy `docs/` to the requested design/book-preview.

Book 2 adds chapters 6–12, moves the visit walkthrough to chapter 13 and the
capstone to 14, and retains the older URLs. There are 16 pages including Appendix A.
The reading order lives in `_pkgdown.yml` and `pkgdown/index.md`; filenames retain
stable URLs rather than encoding the current order for every page.

For a focused edit, `tools/book/run-pod.sh precompute 'book-09-dates'` reuses the
installed pair and executes only that source. The final validation executes every
chapter. `verify-book2.R` checks reducer values, population/episode counts, matching,
format components and joins, death censoring, date types and expected refusal
rollback. Expected refusal messages are printed through `tryCatch`; they are
lessons, not ignored knitting errors. Book 2 evidence lives in `evidence/book2/`.

The completed Book 2 also follows the expanded 14 November brief: chapters 0–4
use GiBleed; chapters 6, 8 and 11 use Eunomia Synthea27Nj (the numeric workstream's
chosen dataset). The other chapters state when they use the 24-person questionnaire
fixture. This fixture remains temporary and is not a third practice resource.
Both practice CDMs are public synthetic data, loaded without changing their rows.
`practice.R` registers both as `gibleed` and `numeric` on one real DSLite server.

Run `Rscript tools/book/load-practice.R ../practice-data` for a portable SQLite
installation. `BOOK_PRACTICE_DIR` can point the helper to that loaded directory.
The same files can be mounted into Rock and registered with the resource URLs in
chapter 0; no data changes are needed to move them to a different host.

The loader writes source archive hashes, revision, counts and provenance to
`manifest.json` in the selected practice-data directory. Appendix A reads that
file, respecting `BOOK_PRACTICE_DIR` and the `../practice-data` default.
`evidence/book2/practice-manifest.json` retains historical build evidence. The loader preserves textual vocabulary
and device identifiers instead of guessing that every `_id` column is numeric.
GiBleed's header-only COST.csv has an incomplete final line; the recorded loader
warning is harmless and the empty table is created with its columns.

Styling follows the [OHDSI web style guide](https://www.ohdsi.org/web/wiki/doku.php?id=development:style_guide)
with its native system font stack, navy/orange/blue palette, and a Bootstrap 5
bslib theme. The chapter navigation and in-page contents remain accessible on
phones. Code and recorded outputs are separate fences; pkgdown supplies code-copy
buttons. Author bylines and the book footer are site metadata; DESCRIPTION is unchanged.

Review-1 fixes: `verify-book2.R` checks the actual released numeric summaries and
count tables against the public synthetic oracle, including the count exercise.
`display.R` formats only successful releases; full responses are retained in
expandable details. `audit-content.py` rejects normal-return failure lists unless
visibly delimited as an expected refusal lesson. The deliberately refused table
in the verification probe must make the formatter stop.

Run `Rscript tools/book/verify-setup.R` with the same pod library after installation.
It executes chapter 2's actual attach/switch chunks against renamed resources in
DSLite and checks the Opal scripts' API signatures. This is not a live Opal test.
The optional provisioning scripts and Compose mount are shipped in the helper
ZIP; chapter 0 states the live-deployment validation limit explicitly.

After building, `Rscript tools/book/verify-bundle.R` unpacks the downloadable ZIP
into a separate empty reader directory for each of the 16 chapters and executes
all chunks, including exercises and solutions, in fresh R processes. Only the
lesson text, runner and expected output are staged outside the ZIP; no checkout
helpers or verification harness are sourced. Every regenerated lesson must match
the precomputed `.Rmd` byte-for-byte. The capstone also checks native plan and
assigned-table equality and cause counts using the public synthetic fixture.
Appendix A runs again with a custom `BOOK_PRACTICE_DIR` and no default data path.
The verifier reuses loader-produced practice data and its manifest via symlinks
and removes its temporary directories afterwards. The ZIP includes the unchanged
released `tools/dates/va-native.R` as well as the nine `tools/book/` helpers.
