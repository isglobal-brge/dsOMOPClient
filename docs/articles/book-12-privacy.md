# 12\. What stays private

**What you will learn**

Distinguish a private assignment from an aggregate release, understand
distinct-person support, and recognise refused identifiers and free
text.

**You need:** the client packages and the Opal login from [Getting
started](https://isglobal-brge.github.io/dsOMOPClient/articles/book-00-start.md).
This chapter uses `omop.questionnaire`, the synthetic questionnaire
study described there.

## Start this chapter

Log in first:

``` r
library(dsOMOPClient)
conns <- ds.omop.login("https://localhost:8443", user = "administrator",
  password = "password", resource = "omop.questionnaire", server = "site")$conns
```

The connection is ready; `conns` identifies the server used by the
analysis calls.

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
private <- omop_recipe(variables = list(age = omop_age_at("2024-06-01"), sex = omop_sex()),
  outputs = omop_output("people", format = "wide", grain = "person"))
recipe_execute(private, out = c(people = "D_private"), datasources = conns)
```

The reply names `D_private`, the server table holding age and sex. Exact
completed age stays beside the source data; the table is available for
approved analysis calls in your DataSHIELD session.

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
    #> [1] 48
    #>
    #> $site$`quantiles & mean`
    #>   5%  10%  25%  50%  75%  90%  95% Mean
    #>   34   34   34   43   49   49   49   42

``` r
dsBaseClient::ds.numNA("D_private$age", datasources = conns)
```

    #> $site
    #> [1] 12

The mean completed age on 2024-06-01 is 42 years across 36 valid values;
12 people with year-only births have missing completed age. The roster
includes all people; a fixed-date age calculation does not establish
survival to that date. An approved aggregate method checks its own
disclosure rules. A successfully assigned table does not guarantee every
later summary or model will be releasable.

**Note**

A private assignment is not a row download. Every later aggregate uses
the disclosure rules of its own method.

## The final release rule

Each final patient component needs enough **distinct persons** for the
server’s `nfilter.subset`. Many events from one person still contribute
only one person to this check. Our server sets `nfilter.subset = 5`,
stricter than the dsBase default of 3. Each data owner chooses the
server policy.

| Rule             | What it checks                                    | Value on this server |
| ---------------- | ------------------------------------------------- | -------------------: |
| `nfilter.subset` | Distinct people in a final assigned patient table |                    5 |
| `nfilter.tab`    | Nonzero cells in a dsBase frequency table         |                    3 |

These checks apply at different steps. A table with enough people to
assign can still produce frequency cells too small to release.

Intermediate calculations may contain small populations privately. The
gate is at final publication; a joined result’s private inputs do not
need separate published symbols. A standalone filter produces a final
object and is gated. Here we intentionally request a subset with no
qualifying rows:

``` r
ds.omop.filter("D_private", expr = omop_where(age > 200L),
  newobj = "D_too_small", datasources = conns)
```

    #> Error: OMOP filter failed and was rolled back: site: Disclosive: operation blocked - insufficient individuals to meet disclosure threshold. No further details available.; There are some DataSHIELD errors, list them with datashield.errors().

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

Fever question is **900002**, found in
[chapter 4](https://isglobal-brge.github.io/dsOMOPClient/articles/book-04-vocabulary.md).

``` r
blocked <- omop_recipe(variables = list(answer = omop_variable(
  event = omop_event("observation", concepts = 900002L, value = "value_as_string"),
  reducer = omop_reduce("first_value"))),
  outputs = omop_output("people", format = "wide", grain = "person"))
recipe_to_plan(blocked)
```

    #> Error: Disclosive: the plan requests blocked (class-1) column(s) that the server can never release or filter on: value_as_string (event 'obs_900002' value).

This definition is refused before execution. Use the reviewed coded
answer `value_as_concept_id` and translated categories when it answers
the question, as in
[chapter 10](https://isglobal-brge.github.io/dsOMOPClient/articles/book-10-harmonise.md).
Free text is not needed for that analysis.

## Why dates and ages can remain private columns

*Class 2* means a quasi-identifier: a value that may help identify
someone when combined with other facts. Exact dates, birth components
and ages are permitted in private server tables. Their aggregates and
any category labels still face the applicable release controls.

``` r
date_recipe <- omop_recipe(variables = list(visit_date = omop_date("first_visit")),
  outputs = omop_output("people", format = "wide", grain = "person"))
recipe_execute(date_recipe, out = c(people = "D_private_date"), datasources = conns)
dsBaseClient::ds.colnames("D_private_date", datasources = conns)
```

    #> $site
    #> [1] "person_id"  "visit_date"

The schema shows `person_id` and `visit_date`; it does not reveal the
dates. Turning an exact date into a factor publishes category labels as
metadata, which needs its own support checks. A fine custom date pattern
can therefore be refused even when a private Date column is permitted.
Coarsen to the scientifically appropriate month/quarter/year and request
an approved aggregate; do not encode a disallowed field into another
name or type.

## Protected keys cannot become analyst variables

``` r
ds.omop.mutate("D_private", copied = person_id,
  newobj = "D_key_copy", datasources = conns)
```

    #> Error: OMOP mutate failed and was rolled back: site: validation_error: omopMutateDS: column 'person_id' is a protected identifier and cannot be used in an expression; There are some DataSHIELD errors, list them with datashield.errors().

The server needs keys to align rows and joins; an analyst mutation
cannot copy, rewrite or filter them. Use the declared population and
join operations instead.

## What the disclosure checks mean

*Class 3* covers ordinary analytic values, such as coded categories and
numeric measurements, rather than identifiers. It does not mean “safe to
download”: these values still remain private and their aggregates face
release checks. The analyses in this book use DataSHIELD disclosure
controls and add no noise. Some OHDSI analysis routes add noise fixed
for each request, so repeating the same request returns the same answer.

## Exercise 1: a nonempty subset can also be too small

Keep female participants whose completed age is 43 on the declared date.
The public questionnaire description places four people in this group.
Predict whether the server can assign that final subset.

Solution

``` r
ds.omop.filter("D_private", expr = omop_where(age == 43 & sex == "F"),
  newobj = "D_small_group", datasources = conns)
```

    #> Error: OMOP filter failed and was rolled back: site: Disclosive: operation blocked - insufficient individuals to meet disclosure threshold. No further details available.; There are some DataSHIELD errors, list them with datashield.errors().

The four-person group is below `nfilter.subset = 5`, so it is refused
just as the empty group was. The message does not disclose the small
count; here the count was known from the public teaching data. In a real
study, retain a scientifically justified larger population or report
that the intended analysis cannot be released.

## Exercise 2: did a refused copy create a table?

After the protected-key refusal, list the server objects. Is
`D_key_copy` present?

Solution

``` r
DSI::datashield.symbols(conns)
```

    #> $site
    #> [1] "D_private"      "D_private_date" "omop"

`D_private` and `D_private_date` still exist, but `D_key_copy`,
`D_too_small` and `D_small_group` are absent. A refused request did not
create a usable table. A schema or object name is metadata, not access
to its patient rows.

## Tables when some categories have very few people

A table is released only when every category it shows at a site holds at
least `nfilter.tab = 3` people there. Otherwise `ds.table()` refuses the
whole table and `ds.summary()` answers “INVALID object\!”. The two
GiBleed sites hold a few rare conditions; log in to them as a second
analysis:

``` r
rare_conns <- ds.omop.login(rep("https://localhost:8443", 2), user = "administrator",
  password = "password", server = c("north", "south"),
  resource = c(north = "omop.gibleed_north", south = "omop.gibleed_south"),
  symbol = "rare")$conns
```

Publish each person’s first rare condition and their sex, and tabulate
it:

``` r
rare <- omop_concept_set(c(4066995, 44782520, 317009, 198199, 80809, 4043241))
first_rare <- omop_variable(event = omop_event("condition_occurrence", name = "rare",
  concepts = rare), reducer = omop_reduce("first_value", value = "condition_concept_id"))
sex <- omop_variable(name = "sex", table = "person", column = "gender_concept_id",
  type = "categorical", format = "raw")
recipe_execute(omop_recipe(variables = list(condition = first_rare, sex = sex),
    outputs = omop_output("rare", format = "wide", grain = "person")),
  out = c(rare = "D_rare"), symbol = "rare", datasources = rare_conns)
ds.omop.table("D_rare", "condition", datasources = rare_conns)
```

    #> All studies failed for reasons identified below
    #> north: Failed: at least one cell has a non-zero count less than nfilter.tab i.e. 3
    #> south: Failed: at least one cell has a non-zero count less than nfilter.tab i.e. 3

Some condition is held by one or two people at a site, so no count of
that table is released there. For a descriptive table, make a copy in
which, at each site, the categories held by fewer than three people are
missing. dsBase’s own table and summary then work on it:

``` r
ds.omop.drop_small_categories("D_rare", newobj = "D_rare_tables", symbol = "rare",
  datasources = rare_conns)
copy <- dsBaseClient::ds.table("D_rare_tables$condition", datasources = rare_conns)
copy$output.list$TABLE_rvar.by.study_counts
```

|                                                   | north | south |
| :------------------------------------------------ | ----: | ----: |
| Rheumatoid\_arthritis                             |     6 |    12 |
| Pyelonephritis                                    |     5 |     0 |
| Familial\_Alzheimer\_s\_disease\_of\_early\_onset |    11 |     7 |
| Asthma                                            |     0 |     4 |
| NA                                                |  1325 |  1324 |

``` r
dsBaseClient::ds.summary("D_rare_tables$condition", datasources = rare_conns)
```

    #> $north
    #> $north$class
    #> [1] "dsomop_concept_factor" "factor"
    #>
    #> $north$length
    #> [1] 1347
    #>
    #> $north$categories
    #> [1] "Rheumatoid_arthritis"
    #> [2] "Pyelonephritis"
    #> [3] "Asthma"
    #> [4] "Familial_Alzheimer_s_disease_of_early_onset"
    #>
    #> $north$`count of 'Rheumatoid_arthritis'`
    #> [1] 6
    #>
    #> $north$`count of 'Pyelonephritis'`
    #> [1] 5
    #>
    #> $north$`count of 'Asthma'`
    #> [1] 0
    #>
    #> $north$`count of 'Familial_Alzheimer_s_disease_of_early_onset'`
    #> [1] 11
    #>
    #>
    #> $south
    #> $south$class
    #> [1] "dsomop_concept_factor" "factor"
    #>
    #> $south$length
    #> [1] 1347
    #>
    #> $south$categories
    #> [1] "Rheumatoid_arthritis"
    #> [2] "Pyelonephritis"
    #> [3] "Asthma"
    #> [4] "Familial_Alzheimer_s_disease_of_early_onset"
    #>
    #> $south$`count of 'Rheumatoid_arthritis'`
    #> [1] 12
    #>
    #> $south$`count of 'Pyelonephritis'`
    #> [1] 0
    #>
    #> $south$`count of 'Asthma'`
    #> [1] 4
    #>
    #> $south$`count of 'Familial_Alzheimer_s_disease_of_early_onset'`
    #> [1] 7

A zero now means “fewer than three people” at that site. When the people
made missing at a site would be only one or two, the next smallest
category is made missing too, so that subtracting the table from a total
never reveals a small count. The copy is for descriptive tables: those
people’s conditions are missing in it. For models, group rare categories
instead (`omop_category_recode()`, `omop_category_rollup()`). To publish
a recipe’s outputs this way from the start, pass `small_categories =
"missing"` to `recipe_execute()`.

For a two-way table, `ds.omop.table()` can make the small cells missing
in its temporary copy; the published table is unchanged:

``` r
ds.omop.table("D_rare", "condition", by = "sex", small = "missing", datasources = rare_conns)
```

    #> Combined (all valid sites): condition by sex
    #>                                                       D_rare$sex
    #> D_rare$condition                                       MALE FEMALE
    #>   Rheumatoid_arthritis                                   10      6
    #>   Pyelonephritis                                          0      9
    #>   Asthma                                                  0      0
    #>   Familial_Alzheimer_s_disease_of_early_onset             8     10
    #>   Fracture_of_vertebral_column_with_spinal_cord_injury    0      0
    #>   Chronic_paralysis_due_to_lesion_of_spinal_cord          0      0
    #>
    #> By site:
    #> TABLE_STUDY.north_counts
    #>                                                       D_rare$sex
    #> D_rare$condition                                       MALE FEMALE
    #>   Rheumatoid_arthritis                                    4      0
    #>   Pyelonephritis                                          0      5
    #>   Asthma                                                  0      0
    #>   Familial_Alzheimer_s_disease_of_early_onset             4      7
    #>   Fracture_of_vertebral_column_with_spinal_cord_injury    0      0
    #>   Chronic_paralysis_due_to_lesion_of_spinal_cord          0      0
    #> TABLE_STUDY.south_counts
    #>                                                       D_rare$sex
    #> D_rare$condition                                       MALE FEMALE
    #>   Rheumatoid_arthritis                                    6      6
    #>   Pyelonephritis                                          0      4
    #>   Asthma                                                  0      0
    #>   Familial_Alzheimer_s_disease_of_early_onset             4      3
    #>   Fracture_of_vertebral_column_with_spinal_cord_injury    0      0
    #>   Chronic_paralysis_due_to_lesion_of_spinal_cord          0      0
    #>
    #> Categories held by fewer than nfilter.tab people at a site are missing at that site.

The labels, such as `Chronic_paralysis_due_to_lesion_of_spinal_cord`,
keep only letters, digits, `.`, `_`, `-` and `+`: Opal’s DataSHIELD
parser refuses spaces, accents and most punctuation inside the requests
that dsBase’s functions send, so published labels are written that way,
identically at every site. `recipe_codebook()` keeps the exact
vocabulary names.

## Recap

A recipe creates private objects and returns their names. Final patient
outputs need distinct-person support; aggregates and category metadata
have further checks. Class-1 fields and protected-key expressions are
refused. A table with categories held by one or two people at a site is
refused there; a copy without them serves descriptive tables.

**Next:** [13. Visit dates, one idea at a
time](https://isglobal-brge.github.io/dsOMOPClient/articles/book-13-visits.md).
