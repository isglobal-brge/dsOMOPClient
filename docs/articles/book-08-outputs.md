# 8\. Choosing an output format

**What you will learn**

Choose a table shape for your question and run one small example of
every format, including explicit survival censoring.

This chapter uses the public synthetic Synthea27Nj numeric connection
from [Getting
started](https://isglobal-brge.github.io/dsOMOPClient/articles/book-00-start.md).

**You need:** the packages and the matching practice connection
described in [Before you
start](https://isglobal-brge.github.io/dsOMOPClient/articles/book-00-start.html#before-you-start).
Run its sections in order. Each chapter starts from a fresh practice
connection; all the outputs below come from real dsOMOP and dsOMOPClient
2.10.5 calls. The numbers describe teaching data, not clinical findings.

Concept selections refer to the searches and saved definitions in
[chapter 4](https://isglobal-brge.github.io/dsOMOPClient/articles/book-04-vocabulary.md),
including the separate questionnaire catalogue. Additional
body-measurement concepts are found on screen in the [formula
walkthrough](https://isglobal-brge.github.io/dsOMOPClient/articles/book-15-formulas.html#find-the-inputs-before-calculating).

## Start this chapter

Start a fresh R session in the `book-work` directory created in [Getting
started](https://isglobal-brge.github.io/dsOMOPClient/articles/book-00-start.html#get-the-book-helpers).
Run this setup first:

``` r
library(dsOMOPClient)
source("tools/book/display.R")
source("tools/book/connection.R")
source("tools/book/practice.R")
practice <- if (book_driver == "DSLite") book_practice("numeric") else book_connect("numeric")
conns <- practice$conns
names(conns)
```

    #> [1] "site"

The `site` connection is ready. The shared `connection.R` selects DSLite
or Opal and resolves the resource names.

## Choose the shape for the analysis

An output controls the shape of a private server table. It never grants
a row download. *Grain* means what one row represents: a person, an
indexed episode, an event or a time period. More rows do not create more
independent people.

| Format                 | Use it for                           | Main row meaning                  |
| ---------------------- | ------------------------------------ | --------------------------------- |
| wide                   | A few named variables, models        | Person or episode                 |
| long                   | Event sequences                      | Event within a population episode |
| features               | A feature table preset               | Wide person or episode            |
| baseline               | Entry characteristics preset         | Wide person or episode            |
| person\_period / panel | Repeated measurements over follow-up | Episode and time bin              |
| temporal\_covariates   | Sparse changes over time             | Nonzero covariate and time bin    |
| survival               | Time until an outcome or censoring   | Follow-up record                  |
| intervals              | Exposure or event spans              | Interval                          |
| sparse                 | Many mostly absent covariates        | Nonzero covariate                 |
| membership             | Reuse population episodes            | Population member episode         |
| dictionary             | Describe concepts used by outputs    | Vocabulary entry                  |
| joined                 | Combine compatible private outputs   | Declared join grain               |

`features` and `baseline` are `type` presets which lower to native wide
outputs; `panel` is the simple-layer name for `person_period`. We check
every resulting recipe selects language 2. Use `format` for the native
formats.

## Prepare one small study

We will reuse these definitions so each example changes only the output.

``` r
visits <- omop_event("visit_occurrence", concepts = c(9201L, 9202L), name = "visits")
rx <- omop_event("visit_occurrence", concepts = c(9201L, 9202L), name = "spans")
inpatient <- omop_event("visit_occurrence", concepts = 9201L, name = "inpatient")
visitors <- omop_population("visitors", index = omop_index(visits))
class(visitors)
```

    #> [1] "omop_population" "list"

Each first visit begins one episode. A later inpatient visit will be our
outcome.

``` r
base <- omop_recipe(events = list(visits, rx, inpatient),
  populations = list(visitors), variables = list(
    visits = omop_variable(event = visits, reducer = omop_reduce("count")),
    age = omop_age(),
    hemoglobin = omop_mean_value("measurement", 3000963L, unit = 8713L,
      window = omop_window(NULL, NULL))),
  outputs = omop_output("wide", rows = "visitors", format = "wide"))
recipe_explain(base)$selected
```

    #> [1] "v2"

The visit count reads all history here. Age uses annual year subtraction
at entry. The template already contains an executable wide output.

## Wide: named columns

``` r
print(recipe_execute(base, out = c(wide = "D_fmt_wide"), conns = conns))
```

    #>         wide
    #> "D_fmt_wide"

``` r
dsBaseClient::ds.colnames("D_fmt_wide", datasources = conns)
```

    #> $site
    #> [1] "row_id"        "cohort_row_id" "person_id"     "visits"        "age"
    #> [6] "hemoglobin"

There is one row per episode and one column per requested variable. A
person’s pseudonym and episode keys are supplied by the server.

## Long: keep event records

Use long when event order and repeated records matter. Select a named
source instead of reducing it.

``` r
long <- base
long$outputs <- list(long = omop_output("long", rows = "visitors", format = "long",
  sources = "visits"))
print(recipe_execute(long, out = c(long = "D_fmt_long"), conns = conns))
```

    #>         long
    #> "D_fmt_long"

``` r
dsBaseClient::ds.colnames("D_fmt_long", datasources = conns)
```

    #> $site
    #> [1] "row_id"                   "cohort_row_id"            "person_id"
    #> [4] "concept_id"               "start_date"               "end_date"
    #> [7] "visit_type_concept_id"    "admitted_from_concept_id" "discharged_to_concept_id"

The source columns become a normalised schema, including `concept_id`
and `start_date`. Dates default to offsets from the index. One source
makes the event-row meaning clear; multiple sources need an explicit
event-log design.

## Features: a wide preset

Use the features preset when a downstream analysis expects a feature
table. Native typed variables preserve their declared values.

``` r
features <- base
features$outputs <- list(features = omop_output("features", rows = "visitors", type = "features"))
print(recipe_execute(features, out = c(features = "D_fmt_features"), conns = conns))
```

    #>         features
    #> "D_fmt_features"

``` r
dsBaseClient::ds.colnames("D_fmt_features", datasources = conns)
```

    #> $site
    #> [1] "row_id"        "cohort_row_id" "person_id"     "visits"        "age"
    #> [6] "hemoglobin"

This is a wide table, not the sparse coordinate representation shown
below. The native selection check prevents an accidental older-language
execution.

## Baseline: characteristics at entry

Use baseline to name an entry-characteristics table. Define the time
windows of its variables explicitly.

``` r
baseline <- base
baseline$outputs <- list(baseline = omop_output("baseline", rows = "visitors", type = "baseline"))
print(recipe_execute(baseline, out = c(baseline = "D_fmt_baseline"), conns = conns))
```

    #>         baseline
    #> "D_fmt_baseline"

``` r
dsBaseClient::ds.colnames("D_fmt_baseline", datasources = conns)
```

    #> $site
    #> [1] "row_id"        "cohort_row_id" "person_id"     "visits"        "age"
    #> [6] "hemoglobin"

The preset does not turn an all-history count into a pre-index count.
Here the `omop_age()` baseline preset uses its banded annual-age
default. Use `omop_age_at()` in a wide table for explicit completed age.
The simple study layer makes baseline an ordinary wide output.

## Panels: one row per follow-up bin

A *bin* is a time interval. These are three 30-day bins after entry, not
calendar months.

``` r
panel <- base
panel$outputs <- list(panel = omop_output("panel", rows = "visitors", format = "person_period",
  options = list(sources = list(visits), bins = list(width = 30L),
    window = omop_window(1, 90), analyses = "count", representation = "dense")))
print(recipe_execute(panel, out = c(panel = "D_fmt_panel"), conns = conns))
```

    #>         panel
    #> "D_fmt_panel"

``` r
dsBaseClient::ds.colnames("D_fmt_panel", datasources = conns)
```

    #> $site
    #> [1] "row_id"                        "cohort_row_id"
    #> [3] "person_id"                     "period_start_days"
    #> [5] "period_end_days"               "days"
    #> [7] "visits_inpatient_visit_count"  "visits_outpatient_visit_count"

A dense panel retains the empty bins with zero event counts. `bins =
list(calendar = "month")` instead aligns to calendar months, whose
lengths vary. Account for repeated rows per person in downstream
analysis.

## Temporal covariates: sparse time bins

Use temporal covariates when you need nonzero covariate values with a
time index. Including day zero gives this example actual nonzero
entries.

``` r
temporal <- base
temporal$outputs <- list(temporal = omop_output("temporal", rows = "visitors", format = "temporal_covariates",
  options = list(sources = list(visits), bin_width = 30L,
    window = omop_window(0, 89), analyses = "count")))
print(recipe_execute(temporal, out = c(temporal = "D_fmt_temporal"), conns = conns))
```

    #> $site
    #> $site$temporal
    #> $site$temporal$temporal
    #> $site$temporal$temporal$symbol
    #> [1] "D_fmt_temporal"
    #>
    #> $site$temporal$temporal$grain
    #> [1] "episode"
    #>
    #> $site$temporal$temporal$n_columns
    #> [1] 6
    #>
    #>
    #> $site$temporal$covariateRef
    #> $site$temporal$covariateRef$symbol
    #> [1] "D_fmt_temporal.covariateRef"
    #>
    #> $site$temporal$covariateRef$grain
    #> [1] "none"
    #>
    #> $site$temporal$covariateRef$n_columns
    #> [1] 5
    #>
    #>
    #> $site$temporal$timeRef
    #> $site$temporal$timeRef$symbol
    #> [1] "D_fmt_temporal.timeRef"
    #>
    #> $site$temporal$timeRef$grain
    #> [1] "none"
    #>
    #> $site$temporal$timeRef$n_columns
    #> [1] 3

``` r
dsBaseClient::ds.colnames("D_fmt_temporal", datasources = conns)
```

    #> $site
    #> [1] "row_id"          "cohort_row_id"   "person_id"       "time_id"
    #> [5] "covariate_id"    "covariate_value"

The returned mapping also names the covariate and time reference tables.
Keep those companions: numeric covariate/time keys need their
definitions. Missing records and missing measurements are not
automatically interchangeable.

## Survival: state the censoring rule

A *time-at-risk window* says when outcomes count. *Censoring* ends
follow-up without recording the outcome. Here follow-up starts the day
after entry and has no fixed upper limit.

``` r
survival <- base
survival$outputs <- list(survival = omop_output("survival", rows = "visitors", format = "survival",
  options = list(outcomes = list(admission = inpatient),
    tar = omop_window(1, NULL), censor = list("observation_end", "death"))))
print(recipe_execute(survival, out = c(survival = "D_fmt_survival"), conns = conns))
```

    #>         survival
    #> "D_fmt_survival"

``` r
dsBaseClient::ds.colnames("D_fmt_survival", datasources = conns)
```

    #> $site
    #> [1] "row_id"                "cohort_row_id"         "person_id"
    #> [4] "outcome_name"          "event"                 "entry_days_from_index"
    #> [7] "exit_days_from_index"  "follow_up_days"

Only the named censor sources cap follow-up: observation end and death
here. The window also limits follow-up when its upper bound is finite.
An episode end is not an extra censor unless requested. A censor is not
a “no outcome ever” assertion. Always state the window: omitting it
would use the 730-day compatibility default.

``` r
survival_result <- dsBaseClient::ds.table("D_fmt_survival$event", useNA = "no", datasources = conns)
knitr::kable(book_counts(survival_result))
```

| Category | Count |
| :------- | ----: |
| 0        |    21 |
| 1        |     7 |

Full aggregate response

``` r
survival_result
```

    #> $output.list
    #> $output.list$TABLE_rvar.by.study_row.props
    #>                     study
    #> D_fmt_survival$event site
    #>                    0    1
    #>                    1    1
    #>
    #> $output.list$TABLE_rvar.by.study_col.props
    #>                     study
    #> D_fmt_survival$event site
    #>                    0 0.75
    #>                    1 0.25
    #>
    #> $output.list$TABLE_rvar.by.study_counts
    #>                     study
    #> D_fmt_survival$event site
    #>                    0   21
    #>                    1    7
    #>
    #> $output.list$TABLES.COMBINED_all.sources_proportions
    #> D_fmt_survival$event
    #>    0    1
    #> 0.75 0.25
    #>
    #> $output.list$TABLES.COMBINED_all.sources_counts
    #> D_fmt_survival$event
    #>  0  1
    #> 21  7
    #>
    #>
    #> $validity.message
    #> [1] "Data in all studies were valid"

Counts of the `event` flag distinguish observed outcomes from censored
follow-up in this one-outcome example.

## Intervals: keep a start and an end

Use intervals for recorded spans. These visit intervals have recorded
start and end dates. The same layout can describe drug exposures or
eras.

``` r
intervals <- base
intervals$outputs <- list(intervals = omop_output("intervals", rows = "visitors", format = "intervals",
  options = list(sources = list(rx), window = omop_window(0, NULL))))
print(recipe_execute(intervals, out = c(intervals = "D_fmt_intervals"), conns = conns))
```

    #>         intervals
    #> "D_fmt_intervals"

``` r
dsBaseClient::ds.colnames("D_fmt_intervals", datasources = conns)
```

    #> $site
    #> [1] "row_id"                "cohort_row_id"         "person_id"
    #> [4] "interval_type"         "concept_id"            "start_days_from_index"
    #> [7] "end_days_from_index"

Both bounds remain private, normally as index-relative days. Overlapping
intervals are not automatically a union of covered days; define an era
or a coverage reducer for that question.

## Sparse: nonzero covariates plus a reference

Use sparse for many covariates when most combinations have no
contribution.

``` r
sparse <- base
sparse$outputs <- list(sparse = omop_output("sparse", rows = "visitors", format = "sparse",
  options = list(sources = list(visits), analyses = c("binary", "count"))))
print(recipe_execute(sparse, out = c(sparse = "D_fmt_sparse"), conns = conns))
```

    #> $site
    #> $site$sparse
    #> $site$sparse$sparse
    #> $site$sparse$sparse$symbol
    #> [1] "D_fmt_sparse"
    #>
    #> $site$sparse$sparse$grain
    #> [1] "episode"
    #>
    #> $site$sparse$sparse$n_columns
    #> [1] 5
    #>
    #>
    #> $site$sparse$covariateRef
    #> $site$sparse$covariateRef$symbol
    #> [1] "D_fmt_sparse.covariateRef"
    #>
    #> $site$sparse$covariateRef$grain
    #> [1] "none"
    #>
    #> $site$sparse$covariateRef$n_columns
    #> [1] 5

``` r
dsBaseClient::ds.colnames("D_fmt_sparse", datasources = conns)
```

    #> $site
    #> [1] "row_id"          "cohort_row_id"   "person_id"       "covariate_id"
    #> [5] "covariate_value"

The data contain a covariate key and value. The reference companion maps
keys to source, concept and analysis. A missing sparse row is not a
missing person roster: retain membership separately. Each published
patient component must meet its own support rule.

## Membership: retain the population roster

Use membership to keep entry/end dates and keys for every episode
independently of its measurements.

``` r
membership <- base
membership$outputs <- list(membership = omop_output("membership", rows = "visitors", format = "membership"))
print(recipe_execute(membership, out = c(membership = "D_fmt_membership"), conns = conns))
```

    #>         membership
    #> "D_fmt_membership"

``` r
dsBaseClient::ds.colnames("D_fmt_membership", datasources = conns)
```

    #> $site
    #> [1] "row_id"            "cohort_row_id"     "person_id"         "cohort_start_date"
    #> [5] "cohort_end_date"   "index_source"

This is still a patient-linked private object. It is not a public list
of participants.

## Dictionary: describe the concepts

Use dictionary to describe vocabulary concepts used by a declared source
output. Here `counts` defines those dependencies privately; only the
dictionary is published. It does not contain patient rows.

``` r
dictionary <- base
dictionary$outputs <- list(
  counts = omop_output("counts", rows = "visitors", format = "wide", columns = "visits"),
  dictionary = omop_output("dictionary", rows = "visitors", format = "dictionary",
    options = list(source_outputs = "counts")))
dictionary$publish <- "dictionary"
print(recipe_execute(dictionary, out = c(dictionary = "D_fmt_dictionary"), conns = conns))
```

    #>         dictionary
    #> "D_fmt_dictionary"

``` r
dsBaseClient::ds.colnames("D_fmt_dictionary", datasources = conns)
```

    #> $site
    #> [1] "concept_id"       "concept_name"     "domain_id"        "vocabulary_id"
    #> [5] "concept_class_id" "source"

The schema contains vocabulary fields, not frequencies. A dictionary
does not certify that a concept occurred in a particular person, and it
is not a replacement for the sparse covariate reference.

## Joined: combine private outputs

A native joined output combines declared output dependencies, using
their protected keys and a checked cardinality. It is not an arbitrary
database join. Keep input columns disjoint and state how many rows can
match.

``` r
joined <- base
joined$outputs <- list(
  counts = omop_output("counts", rows = "visitors", format = "wide", columns = "visits"),
  ages = omop_output("ages", rows = "visitors", format = "wide", columns = "age"),
  joined = omop_output("joined", rows = "visitors", format = "joined",
    options = list(from = list(list(output = "counts"), list(output = "ages")),
      how = "left", cardinality = "one_to_one")))
joined$publish <- "joined"
recipe_explain(joined)$selected
```

    #> [1] "v2"

Only the final joined output is published. The two input outputs are
private intermediate steps and do not each need a separately published
symbol.

``` r
print(recipe_execute(joined, out = c(joined = "D_fmt_joined"), conns = conns))
```

    #>         joined
    #> "D_fmt_joined"

``` r
dsBaseClient::ds.colnames("D_fmt_joined", datasources = conns)
```

    #> $site
    #> [1] "row_id"        "cohort_row_id" "person_id"     "visits"        "age"

A left join retains the first input’s rows. Here each episode has
exactly one counts row and one ages row. Repeated event rows would
require a different, explicit cardinality; do not silently multiply a
study population.

## Try it

Summarise the visit-count column of the joined output. Why is it the
same as the wide count?

Solution

``` r
solution_result <- dsBaseClient::ds.summary("D_fmt_joined$visits", datasources = conns)
knitr::kable(book_summary(solution_result))
```

| Site | Statistic |     Value |
| :--- | :-------- | --------: |
| site | 5%        |  11.75000 |
| site | 10%       |  16.40000 |
| site | 25%       |  23.75000 |
| site | 50%       |  45.00000 |
| site | 75%       |  75.25000 |
| site | 90%       | 117.60000 |
| site | 95%       | 152.20000 |
| site | Mean      |  61.96429 |

Full aggregate response

``` r
solution_result
```

    #> $site
    #> $site$class
    #> [1] "integer"
    #>
    #> $site$length
    #> [1] 28
    #>
    #> $site$`quantiles & mean`
    #>        5%       10%       25%       50%       75%       90%       95%      Mean
    #>  11.75000  16.40000  23.75000  45.00000  75.25000 117.60000 152.20000  61.96429

Both use the same reducer and episodes. Joining one row to one row adds
a column without duplicating those counts.

## Another numeric exercise

Summarise the person-level hemoglobin mean in the wide output. Would an
average of a long table’s measurement rows give each person equal
weight?

Solution

``` r
output_lab <- dsBaseClient::ds.summary("D_fmt_wide$hemoglobin", datasources = conns)
knitr::kable(book_summary(output_lab))
```

| Site | Statistic |    Value |
| :--- | :-------- | -------: |
| site | 5%        | 12.90500 |
| site | 10%       | 13.14667 |
| site | 25%       | 13.78500 |
| site | 50%       | 14.32500 |
| site | 75%       | 15.33542 |
| site | 90%       | 15.70000 |
| site | 95%       | 15.92375 |
| site | Mean      | 14.46679 |

The wide table gives each included person one mean in g/dL. An average
over measurement rows would give more weight to people with more
measurements. [Working with lab
values](https://isglobal-brge.github.io/dsOMOPClient/articles/book-14-labs.md)
builds this distinction into a two-connection analysis. Neither shape
grants access to individual rows.

[Chapter 16](https://isglobal-brge.github.io/dsOMOPClient/articles/book-16-longitudinal.md)
develops these choices through irregular measurements, server reshaping,
a discrete-time hazard and pooled mixed models.

## Exercise 2: a common variant

Inspect the wide schema before proposing a model.

Solution 2

``` r
dsBaseClient::ds.colnames("D_fmt_wide", datasources = conns)
```

    #> $site
    #> [1] "row_id"        "cohort_row_id" "person_id"     "visits"        "age"
    #> [6] "hemoglobin"

One column per variable and one row per declared population unit are
modelling choices, not export permissions.

## Recap

Choose the population and row meaning before the format. Outputs stay
private; multi-component outputs carry reference companions. Censoring
and join cardinality are study decisions.

**Next:** [9. Dates, periods and
ages](https://isglobal-brge.github.io/dsOMOPClient/articles/book-09-dates.md).
