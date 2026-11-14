# Appendix A. Reproduction and migration notes

**What you will learn**

Rebuild the offline book and recognise the old recipe conventions.

This chapter uses the 24-person synthetic questionnaire test fixture
described in [Getting
started](https://isglobal-brge.github.io/dsOMOPClient/articles/book-00-start.md).
**You need:** the packages and the matching practice connection
described in [Before you
start](https://isglobal-brge.github.io/dsOMOPClient/articles/book-00-start.html#before-you-start).
Run the sections in order in one R session. All printed results below
were computed with dsOMOP and dsOMOPClient 2.10.4. They are teaching
data, not clinical findings.

## Start this chapter

Start a fresh R session in the `book-work` directory created in [Getting
started](https://isglobal-brge.github.io/dsOMOPClient/articles/book-00-start.html#get-the-book-helpers).
Run this setup first:

``` r
library(dsOMOPClient)
source("tools/book/display.R")
source("tools/book/fixture.R")
practice <- book_fixture()
conns <- practice$conns
names(conns)
```

    #> [1] "site"

The `site` connection is ready. This chapter creates its temporary
questionnaire fixture locally.

## Source and rendered lessons

The editable chapters are `vignettes/book-*.Rmd.orig`. The precompute
script executes them against the synthetic fixture and writes `.Rmd`
with the actual output embedded. The resulting files contain ordinary
Markdown code fences, so the website does not connect to a database when
it builds.

From the book client checkout, run `tools/book/run-pod.sh` in this
workspace, or run `Rscript tools/book/precompute.R` in an R environment
containing the matching packages and fixture dependencies. Then call
`pkgdown::build_site()`. The pod driver is workspace-specific; the R
scripts and fixture are portable.

## Record the software versions

``` r
vapply(c("dsOMOP", "dsOMOPClient", "DSI", "DSLite", "dsBase", "dsBaseClient"),
       function(p) as.character(packageVersion(p)), character(1))
```

    #>       dsOMOP dsOMOPClient          DSI       DSLite       dsBase dsBaseClient 
    #>     "2.10.4"     "2.10.4"      "1.8.0"      "1.5.0"      "6.3.5"      "6.3.5"

These are the actual versions used for this rendering. Detailed
assertions, source hashes and test logs are retained under
`tools/book/evidence`.

## Reproduce both practice resources

Run `Rscript tools/book/load-practice.R ../practice-data`. The loader
pins the EunomiaDatasets source revision and verifies SHA-256 for both
archives. It creates `gibleed.sqlite`, `numeric.sqlite` and a manifest.
These are administrator-side operations on public synthetic data,
separate from analyst DataSHIELD calls. The following table reads the
manifest recorded by the actual completed loader:

``` r
manifest <- jsonlite::read_json("tools/book/evidence/book2/practice-manifest.json")
loaded <- do.call(rbind, lapply(manifest, function(x) data.frame(
  Dataset = x$dataset, Measurements = x$measurements$rows,
  Numeric = x$measurements$numeric_values,
  Numeric_with_unit = x$measurements$numeric_with_unit)))
knitr::kable(loaded, row.names = FALSE)
```

| Dataset     | Measurements | Numeric | Numeric\_with\_unit |
| :---------- | -----------: | ------: | ------------------: |
| GiBleed     |        44053 |       0 |                   0 |
| Synthea27Nj |        10040 |    9107 |                8403 |

GiBleed supplies the basics; Synthea27Nj supplies numeric labs. Its
numeric values and mapped units come from the pinned source, without
fabricated lab extensions. The questionnaire fixture remains separate
and is explicitly named in its lessons.

The
[loader](https://isglobal-brge.github.io/dsOMOPClient/downloads/load-practice.R),
[two-resource DSLite
helper](https://isglobal-brge.github.io/dsOMOPClient/downloads/practice.R)
and [questionnaire
fixture](https://isglobal-brge.github.io/dsOMOPClient/downloads/fixture.R)
are downloadable from this site. The same SQLite files can be mounted
into another Rock server; their resource registration is shown in
chapter 0. Dependencies are listed in `tools/book/README.md`.

## The local test driver

DSLite 1.5 rejects some quoted dsBase call strings containing commas.
The practice helper applies the same scoped lexer repair used by the
package’s release tests. It changes tokenisation only: ds.table,
assignments and all disclosure checks still run for real. It does not
apply to Opal. The two intentional refusal chunks in chapter 12 omit
DSLite’s extra missing-symbol cleanup warnings; they display the actual
policy error, and the verification checks that no refused output exists.
The helper also injects a synthetic test pseudonym root; a deployed
server manages its own private root automatically.

## Migrating older recipes

The 2.10.2 colleague example selected a categorical observation with
`omop_variable(table = ..., concept_id = ..., format = "last_value",
value_source = ...)`. Its translation path required an output name
ending in `_concept_id`. The death feature needed `value_source =
"cause_concept_id"`; `column` alone did not select the reduced value.
Those are legacy conventions, not the API taught here.

In language 2, use `event = omop_event(...)` and `reducer =
omop_reduce("last_value", value = "value_as_concept_id")`. Use `format =
"wide"` and `grain = "person"` on the output. Concept typing now
supplies labels regardless of the output name. Re-execute an old recipe
to obtain new labelled assignments; renaming an existing symbol does not
recompute it.

Calendar migration also matters. `first_date` and `last_date` remain day
offsets. For calendar values use `omop_date()`, for calendar precision
use `omop_date_spec()`, and for completed age at death use
`omop_age_at("death")`. Do not relabel annual year subtraction as
completed age.

## Inspect the executed language

``` r
check <- omop_recipe(variables = list(age = omop_age_at("death")),
  outputs = omop_output("people", format = "wide", grain = "person"))
recipe_explain(check, conns = conns)$selected
```

    #> [1] "v2"

``` r
stopifnot(recipe_explain(check, conns = conns)$selected == "v2")
print(recipe_execute(check, out = c(people = "D_migration"), conns = conns))
```

    #>        people 
    #> "D_migration"

If an explanation selects v1, it is explaining the legacy execution
path. Merely printing a forced v2 conversion would not prove that the
actual execution uses it.

## Recap

The editable source executes once; the generated website is offline.
Migration changes the recipe definitions, not just column names.

## Try it

Check that the migration recipe is native before using it.

Solution

``` r
recipe_explain(check, conns = conns)$selected
```

    #> [1] "v2"
