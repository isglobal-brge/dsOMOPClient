# 12\. What stays private

**What you will learn**

Distinguish a private assignment from an aggregate release, understand
distinct-person support, and recognise refused identifiers and free
text.

This chapter uses the 24-person synthetic questionnaire test fixture
described in [Getting
started](https://isglobal-brge.github.io/dsOMOPClient/articles/book-00-start.md).

**You need:** the packages and the matching practice connection
described in [Before you
start](https://isglobal-brge.github.io/dsOMOPClient/articles/book-00-start.html#before-you-start).
Run its sections in order. Each chapter starts from a fresh practice
connection; all the outputs below come from real dsOMOP and dsOMOPClient
2.10.4 calls. The numbers describe teaching data, not clinical findings.

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

## Know which side holds each object

| On your computer                                | On each server                                  |
| ----------------------------------------------- | ----------------------------------------------- |
| Recipe definitions, concept sets and parameters | Database records and credentials                |
| Public table/column/vocabulary metadata         | Pseudonymous person and episode rows            |
| Output symbol names                             | Exact permitted dates, ages and derived columns |
| Disclosure-checked aggregates                   | Intermediate populations and joins              |

A *symbol* is the name of a server object. Seeing the name does not give
you its contents. Pseudonymisation replaces person IDs for protected
processing; it does not make a patient-level table suitable for
download.

``` r
private <- omop_recipe(variables = list(age = omop_age_at("2024-06-01")),
  outputs = omop_output("people", format = "wide", grain = "person"))
stopifnot(recipe_explain(private, conns = conns)$selected == "v2")
print(recipe_execute(private, out = c(people = "D_private"), conns = conns))
```

    #>      people 
    #> "D_private"

The mapping is the result you receive from recipe execution. Exact
completed age stays beside the source data. This is called publication
into the DataSHIELD session; it is not public publication on the
internet or a row export.

**You**Choose events, variables and outputs.

→

**Server**Compute with private records and apply release checks.

→

**You see**Symbol names, schemas and checked summaries.

## Request an aggregate

``` r
dsBaseClient::ds.summary("D_private$age", datasources = conns)
```

    #> $site
    #> $site$class
    #> [1] "integer"
    #> 
    #> $site$length
    #> [1] 24
    #> 
    #> $site$`quantiles & mean`
    #>   5%  10%  25%  50%  75%  90%  95% Mean 
    #>   34   34   34   43   49   49   49   42

An approved aggregate method checks its own disclosure rules. A
successfully assigned table does not guarantee every later summary or
model will be releasable. The book’s outputs are genuine results of
those methods, not downloaded rows.

**Note**

A private assignment is not a row download. Every later aggregate uses
the disclosure rules of its own method.

## The final release rule

Each final patient component needs enough **distinct persons** for the
server’s `nfilter.subset`. Many events from one person still contribute
only one person to this check. Our fixture uses the ordinary threshold
of five. Thresholds are server policy, not something the analyst lowers
to obtain a desired answer.

Intermediate calculations may contain small populations privately. The
gate is at final publication; a joined result’s private inputs do not
need separate published symbols. A standalone filter produces a final
object and is gated. Here we intentionally request a subset with no
qualifying rows:

``` r
small_message <- tryCatch({
  ds.omop.filter("D_private", expr = omop_where(age > 200L),
    newobj = "D_too_small", conns = conns)
  "Unexpected success"
}, error = function(e) conditionMessage(e))
cat(small_message, "\n")
```

    #> OMOP filter failed and was rolled back: site: Disclosive: operation blocked - insufficient individuals to meet disclosure threshold. No further details available.; There are some DataSHIELD errors, list them with datashield.errors().

The refusal reports the policy boundary without providing the small
private count. The previous `D_private` object remains available. Empty
and undersized results share this protection; do not interpret the
diagnostic as a patient count.

## Why identifiers and source text are refused

*Class 1* includes direct identifiers, source identifiers and free text
that may contain identifiers. Renaming a source field does not make it
safe. Both output values and predicates are checked, so a count cannot
be used to probe a forbidden text column. The following intentionally
invalid definition demonstrates that check:

``` r
text_message <- tryCatch({
  blocked <- omop_recipe(variables = list(answer = omop_variable(
    event = omop_event("observation", concepts = 900002L, value = "value_as_string"),
    reducer = omop_reduce("first_value"))),
    outputs = omop_output("people", format = "wide", grain = "person"))
  recipe_to_plan(blocked)
  "Unexpected success"
}, error = function(e) conditionMessage(e))
cat(text_message, "\n")
```

    #> Disclosive: the plan requests blocked (class-1) column(s) that the server can never release or filter on: value_as_string (event 'obs_900002' value).

This definition is refused before execution. Use the reviewed coded
answer `value_as_concept_id` and translated categories when it answers
the question, as in
[chapter 10](https://isglobal-brge.github.io/dsOMOPClient/articles/book-08-harmonise.md).
Free text is not needed for that analysis.

## Why dates and ages can remain private columns

*Class 2* means a quasi-identifier: a value that may help identify
someone when combined with other facts. Exact dates, birth components
and ages are permitted on these private assignment paths, without an
owner-side date switch. Their aggregates and any category labels still
face the applicable release controls.

``` r
date_recipe <- omop_recipe(variables = list(visit_date = omop_date("first_visit")),
  outputs = omop_output("people", format = "wide", grain = "person"))
stopifnot(recipe_explain(date_recipe, conns = conns)$selected == "v2")
print(recipe_execute(date_recipe, out = c(people = "D_private_date"), conns = conns))
```

    #>           people 
    #> "D_private_date"

``` r
dsBaseClient::ds.colnames("D_private_date", datasources = conns)
```

    #> $site
    #> [1] "person_id"  "visit_date"

You see the schema, not the dates. Turning an exact date into a factor
publishes category labels as metadata, which needs its own support
checks. A fine custom date pattern can therefore be refused even when a
private Date column is permitted. Coarsen to the scientifically
appropriate month/quarter/year and request an approved aggregate; do not
encode a disallowed field into another name or type.

## Protected keys cannot become analyst variables

``` r
key_message <- tryCatch({
  ds.omop.mutate("D_private", copied = person_id,
    newobj = "D_key_copy", conns = conns)
  "Unexpected success"
}, error = function(e) conditionMessage(e))
cat(key_message, "\n")
```

    #> OMOP mutate failed and was rolled back: site: validation_error: omopMutateDS: column 'person_id' is a protected identifier and cannot be used in an expression; There are some DataSHIELD errors, list them with datashield.errors().

The server needs keys to align rows and joins; an analyst mutation
cannot copy, rewrite or filter them. Use the declared population and
join operations instead.

## Recipe protection is not a DP promise

The recipes, table assignments and dsBase analyses in this book use
ordinary DataSHIELD disclosure controls. They do not acquire a
differential-privacy (DP) guarantee just because dsOMOP also provides
protected OHDSI release routes. Those routes have separate contracts and
provenance; inspect them when using those services. Deterministic noise
is not a reason to repeat a request until an answer changes. There are
no per-analyst call budgets or history-dependent answers in this design.

No chapter needs raw-row download methods, weaker thresholds, database
TLS changes or owner configuration to make its normal protected recipe
route work.

## Recap

A recipe creates private objects and returns their names. Final patient
outputs need distinct-person support; aggregates and category metadata
have further checks. Class-1 fields and protected-key expressions are
refused.

## Try it

After the refused small filter, confirm you can still summarise the
original private age column.

Solution

``` r
dsBaseClient::ds.summary("D_private$age", datasources = conns)
```

    #> $site
    #> $site$class
    #> [1] "integer"
    #> 
    #> $site$length
    #> [1] 24
    #> 
    #> $site$`quantiles & mean`
    #>   5%  10%  25%  50%  75%  90%  95% Mean 
    #>   34   34   34   43   49   49   49   42

The original object remains usable. A refusal is not permission to
bypass its release rule with another encoding.
