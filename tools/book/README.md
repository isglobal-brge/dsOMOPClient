# Reproducing the book

Use the locally released 2.10.4 pair. From the client package root, run
`tools/book/run-pod.sh`. It installs only the book pair in its private pod library
and runs `precompute.R`. The pod also needs dsBase/dsBaseClient (the release test
library `/root/rt/labels-deps`), DSLite, DBI, RSQLite, resourcer, knitr and withr.

The source is `vignettes/book-*.Rmd.orig`. Knitr executes it against the
synthetic SQLite fixture and writes `.Rmd` containing ordinary fenced code and
recorded output. Thus `pkgdown::build_site()` needs no live database. Each chapter
gets a fresh connection; its hidden setup establishes only the documented
prerequisites. Assertions in `verify.R` inspect synthetic server memory only;
none of those row-reading calls is an analyst lesson.

The fixture extends the released `helper-dates-va.R`: 24 synthetic people,
six copies of four patterns, with inpatient/outpatient concepts and a second
fever answer. It retains threshold 5, ordinary factor gates and pseudonyms.
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
