# 9\. Reproduction and migration notes

**What you will learn**

Rebuild the offline book and recognise the old recipe conventions.

This chapter uses the synthetic practice connection from [Getting
started](https://isglobal-brge.github.io/dsOMOPClient/articles/book-00-start.md).
Run the sections in order in one R session. All printed results below
were computed with dsOMOP and dsOMOPClient 2.10.4. They are teaching
data, not clinical findings.

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
#>       dsOMOP dsOMOPClient          DSI       DSLite       dsBase dsBaseClient 
#>     "2.10.4"     "2.10.4"      "1.8.0"      "1.5.0"      "6.3.5"      "6.3.5"
```

These are the actual versions used for this rendering. Detailed
assertions, source hashes and test logs are retained under
`tools/book/evidence`.

## The local test driver

DSLite 1.5 rejects some quoted dsBase call strings containing commas.
The practice helper applies the same scoped lexer repair used by the
package’s release tests. It changes tokenisation only: ds.table,
assignments and all disclosure checks still run for real. It does not
apply to Opal. The helper also injects a synthetic test pseudonym root;
a deployed server manages its own private root automatically.

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
#> [1] "v2"
stopifnot(recipe_explain(check, conns = conns)$selected == "v2")
print(recipe_execute(check, out = c(people = "D_migration"), conns = conns))
#>        people 
#> "D_migration"
```

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
#> [1] "v2"
```
