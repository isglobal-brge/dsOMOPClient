# Reproducing the book

Use the locally released 2.10.4 pair. From the client package root, run
`tools/book/run-pod.sh`. It installs only the book pair in its private pod library
and runs `precompute.R`. The pod also needs dsBase/dsBaseClient (the release test
library `/root/rt/labels-deps`), DSLite, DBI, RSQLite, resourcer, knitr and withr.

The source is `vignettes/articles/book-*.Rmd.orig`. Knitr executes it against the
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
