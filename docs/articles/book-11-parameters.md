# 11\. Reusable studies and standalone verbs

**What you will learn**

Bind typed parameters, use omop\_study and shortcuts, then derive and
filter columns without bringing rows to your computer.

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

## Describe the setting as a parameter

A *parameter* is a named, typed placeholder in a study definition. It
lets you reuse a recipe without editing its logic. It is not R code or
SQL text.

``` r
setting <- omop_param("setting", type = "concept_set")
class(setting)
```

    #> [1] "omop_param" "list"

This parameter accepts concept sets. A separate date parameter would
declare a date type; give each changing input its own appropriate type.

## Build a template

``` r
template <- omop_study(omop_event("visit_occurrence", concepts = setting),
  outputs = "baseline", baseline = list(
    prior_visits = omop_count("visit_occurrence", window = omop_window(-365, -1)),
    age = omop_age()))
class(template)
```

    #> [1] "omop_recipe" "list"

`omop_study()` returns an ordinary recipe. It creates an indexed
population and baseline output. We explicitly exclude day zero from the
prior-visit count. The unbound template cannot execute until its
required setting is supplied.

## Bind and inspect

``` r
outpatient <- recipe_bind(template, setting = 9202L)
class(outpatient)
```

    #> [1] "omop_recipe" "list"

Binding fills the placeholder while preserving the template. Concept
9202 is the outpatient visit setting, not the record’s provenance type.

``` r
book_symbols(recipe_execute(outpatient, out = c(baseline = "D_param_outpatient"), conns = conns))
```

    #>             baseline 
    #> "D_param_outpatient"

``` r
dsBaseClient::ds.summary("D_param_outpatient$age", datasources = conns)
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
    #>  0.000000  0.000000  0.750000  3.500000 17.000000 18.000000 18.000000  8.071429

The server computes annual age (index year minus birth year) at the
first outpatient visit. The analyst receives only the approved summary.

## Read the simple-layer defaults

``` r
print(outpatient)
```

    #> Concept presentation: factors; identity is the declared concept class, preserved through aliases and value-preserving reducers.
    #> Label precedence: variable recode, recipe recode, vocabulary name, decimal ID fallback. Explicit equal recodes merge categories.
    #> Federation: permitted ID union, including declared unused levels; newest comparable vocabulary version, then bytewise name tie. Unresolved versions never establish recency.
    #> Generated names: ASCII snake_case vocabulary tokens; colliding automatic names gain concept-ID suffixes. Vocabulary-dependent names are unresolved until permitted federation reconciliation.
    #> variables.prior_visits: concept origin follows reducer count over the declared source value; explicit variable name is retained. 
    #> === omop_recipe ===
    #> Populations ( 2 ):
    #>    base  :  All Persons 
    #>    study  :  Base Population 
    #> Variables ( 2 ):
    #>    prior_visits :  count of visit_occurrence 
    #>    age :  derived age (exact annual age) 
    #> Filters ( 0 ):
    #> Outputs ( 1 ):
    #>    baseline  [ wide ] pop: study 
    #> Study 'study' (omop_study()) defaults chosen:
    #>   - index selection "first" (default): the first candidate per person, selected before the requirements
    #>   - observation coverage unique/drop/drop (default)
    #>   - every output has episode grain; survival ties follow the priority rule; dates are relative to the index date (simple-layer choices)

The study’s default index selection is `first`. The population uses the
default coverage policy; all requirements are combined with AND.
Baseline has episode rows. For survival, supply outcomes and an explicit
follow-up definition. For example, `follow_up = list(start = 1, end
= 365, censor = c("observation_end", "death"))` describes a year after
entry, ending sooner at a listed censor. The [output
chapter](https://isglobal-brge.github.io/dsOMOPClient/articles/book-08-outputs.md)
executes the corresponding native survival format.

## Shortcuts expand into ordinary variables

Use a shortcut when its defaults match your question; use an explicit
reducer when you need more control. `before = 365` includes day zero,
while `after = 365` starts at day one. Without either, most shortcuts
use all history through entry.

``` r
shortcuts <- omop_study(omop_event("measurement", 3000963L), outputs = "baseline",
  baseline = list(ever = omop_ever("measurement", 3000963L, before = 365),
    count = omop_count("measurement", 3000963L, before = 365),
    value = omop_value_at_index("measurement", 3000963L,
      tolerance = c(-30, 0), unit = 8713L)))
book_symbols(recipe_execute(shortcuts, out = c(baseline = "D_shortcuts"), conns = conns))
```

    #>      baseline 
    #> "D_shortcuts"

``` r
dsBaseClient::ds.table("D_shortcuts$ever", useNA = "no", datasources = conns)
```

    #> $output.list
    #> $output.list$TABLE_rvar.by.study_row.props
    #>                 study
    #> D_shortcuts$ever site
    #>                1    1
    #> 
    #> $output.list$TABLE_rvar.by.study_col.props
    #>                 study
    #> D_shortcuts$ever site
    #>                1    1
    #> 
    #> $output.list$TABLE_rvar.by.study_counts
    #>                 study
    #> D_shortcuts$ever site
    #>                1   28
    #> 
    #> $output.list$TABLES.COMBINED_all.sources_proportions
    #> D_shortcuts$ever
    #> 1 
    #> 1 
    #> 
    #> $output.list$TABLES.COMBINED_all.sources_counts
    #> D_shortcuts$ever
    #>  1 
    #> 28 
    #> 
    #> 
    #> $validity.message
    #> [1] "Data in all studies were valid"

Everyone has ever = 1 because the index hemoglobin measurement itself is
included. To ask about measurements strictly before entry use `window =
omop_window(-365, -1)` instead. First/last/mean/min/max-value shortcuts
use the same event and window model. `omop_history()` bundles named ever
variables; it does not define a new language.

## What the mutate contract means

`ds.omop.mutate()` creates or replaces columns on a private
`omop.table`. Although its notation looks like R, the client sends a
**mutate** data request: the server validates a closed grammar and
column types. It never runs arbitrary R code. Bare names refer to
columns, including earlier assignments in the same call; literal
numbers, strings, booleans and missing values are allowed.

The grammar supports `+`, `-`, `*`, `/`, comparisons of a column with a
literal, `ifelse`, `case_when`, `coalesce`, and `cut` with public
breaks. Date subtraction returns days; adding an integer to a date
returns a date. Division by zero or nonfinite arithmetic gives missing
values. Person, episode and set keys and other protected columns cannot
be read or written; class-1 source names are also refused.
[Privacy](https://isglobal-brge.github.io/dsOMOPClient/articles/book-12-privacy.md)
explains those protections.

For derivation inside a new recipe, prefer the native expressions taught
in [Derived numeric
formulas](https://isglobal-brge.github.io/dsOMOPClient/articles/book-15-formulas.md).
This chapter deliberately teaches standalone mutation of an **already
assigned** table, including sequential assignments, new objects and
rollback. Those remain useful operations.

## Prepare weight and height at the same date

BMI is weight in kilograms divided by squared height in metres. Index
each person at their first weight measurement, then select weight and
height on that date. We require the recorded kg and cm units; we do not
combine unrelated latest readings or silently substitute a height from
another day.

``` r
weight_event <- omop_event("measurement", 3025315L)
body <- omop_study(weight_event, outputs = "baseline", baseline = list(
  weight_kg = omop_value_at_index("measurement", 3025315L,
    tolerance = c(0, 0), unit = 9529L),
  height_cm = omop_value_at_index("measurement", 3036277L,
    tolerance = c(0, 0), unit = 8582L),
  age = omop_age_at(omop_anchor(weight_event)),
  first_date = omop_date(omop_anchor(weight_event, which = "first")),
  last_date = omop_date(omop_anchor(weight_event, which = "last"))))
book_symbols(recipe_execute(body, out = c(baseline = "D_body"), conns = conns))
```

    #> baseline 
    #> "D_body"

These are synthetic Synthea measurements, including children. Calculated
BMI is a numeric teaching example, not an adult clinical classification.

## Create a new object

``` r
print(ds.omop.mutate("D_body", height_m = height_cm / 100,
  bmi = weight_kg / (height_m * height_m), newobj = "D_bmi", conns = conns))
```

    #> [1] "D_bmi"

``` r
bmi_result <- dsBaseClient::ds.summary("D_bmi$bmi", datasources = conns)
knitr::kable(book_summary(bmi_result))
```

| Site | Statistic |    Value |
| :--- | :-------- | -------: |
| site | 5%        | 11.53374 |
| site | 10%       | 13.62216 |
| site | 25%       | 15.88957 |
| site | 50%       | 23.86380 |
| site | 75%       | 27.93787 |
| site | 90%       | 30.20849 |
| site | 95%       | 30.36386 |
| site | Mean      | 22.11315 |

The second assignment uses the newly created `height_m`. A missing input
gives a missing BMI. `newobj = "D_bmi"` preserves `D_body`; neither
table is downloaded.

``` r
body_columns <- dsBaseClient::ds.colnames("D_body", datasources = conns)
bmi_columns <- dsBaseClient::ds.colnames("D_bmi", datasources = conns)
c(original_has_bmi = "bmi" %in% body_columns$site,
  new_has_bmi = "bmi" %in% bmi_columns$site)
```

    #> original_has_bmi      new_has_bmi 
    #>            FALSE             TRUE

The original has no BMI column, and the new object does.

## Group ages with public breaks

Choose boundaries from the study design, not from private
minimum/maximum ages. These broad age bands are for teaching. `right =
FALSE` includes the lower bound and excludes the upper bound; ages
outside the range remain missing.

``` r
print(ds.omop.mutate("D_bmi", age_group = cut(age, c(0, 18, 121),
  labels = c("under 18", "18 to 120"), right = FALSE),
  newobj = "D_grouped", conns = conns))
```

    #> [1] "D_grouped"

``` r
age_counts <- dsBaseClient::ds.table("D_grouped$age_group",
  useNA = "always", datasources = conns)
knitr::kable(book_counts(age_counts))
```

| Category  | Count |
| :-------- | ----: |
| 18 to 120 |    14 |
| under 18  |    14 |
| NA        |     0 |

`cut` produces text labels in this grammar, not an ordered R factor. The
table may display them alphabetically.
[Chapter 14](https://isglobal-brge.github.io/dsOMOPClient/articles/book-14-labs.md)
shows how to assign an explicit factor level sequence for modelling and
display.

**Exercise:** create an adult flag using the same lower boundary of 18.

Solution — an age recode

``` r
print(ds.omop.mutate("D_grouped", adult = ifelse(age >= 18, "yes", "no"),
  newobj = "D_adults", conns = conns))
```

    #> [1] "D_adults"

``` r
adult_counts <- dsBaseClient::ds.table("D_adults$adult", useNA = "always",
  datasources = conns)
knitr::kable(book_counts(adult_counts))
```

| Category | Count |
| :------- | ----: |
| no       |    14 |
| yes      |    14 |
| NA       |     0 |

A missing age stays missing rather than becoming “no”. The adult count
equals the 18-to-120 band; the under-18 band becomes “no”.

## Subtract dates on the server

``` r
print(ds.omop.mutate("D_bmi", span_days = last_date - first_date,
  newobj = "D_span", conns = conns))
```

    #> [1] "D_span"

``` r
span_result <- dsBaseClient::ds.summary("D_span$span_days", datasources = conns)
knitr::kable(book_summary(span_result))
```

| Site | Statistic |    Value |
| :--- | :-------- | -------: |
| site | 5%        |  952.700 |
| site | 10%       | 1845.200 |
| site | 25%       | 6142.500 |
| site | 50%       | 6678.000 |
| site | 75%       | 6996.500 |
| site | 90%       | 7049.000 |
| site | 95%       | 7053.550 |
| site | Mean      | 5679.607 |

This is elapsed time between first and last recorded weights, in days.
It is not age or duration under continuous observation. Exact dates stay
private.

**Exercise:** express that duration in 365-day years, retaining the days
column.

Solution — convert a duration

``` r
print(ds.omop.mutate("D_span", span_years = span_days / 365,
  newobj = "D_years", conns = conns))
```

    #> [1] "D_years"

``` r
years_result <- dsBaseClient::ds.summary("D_years$span_years", datasources = conns)
knitr::kable(book_summary(years_result))
```

| Site | Statistic |     Value |
| :--- | :-------- | --------: |
| site | 5%        |  2.610137 |
| site | 10%       |  5.055343 |
| site | 25%       | 16.828767 |
| site | 50%       | 18.295890 |
| site | 75%       | 19.168493 |
| site | 90%       | 19.312329 |
| site | 95%       | 19.324794 |
| site | Mean      | 15.560567 |

Each summary is the day summary divided by 365. These are fixed-length
units, not completed birthdays; use `omop_age_at` for completed age.

## Recode with case\_when

Cases are checked in order. The first true condition wins. In mutate, an
unknown condition gives a missing result immediately, rather than
falling through to the default. Handle missingness explicitly when your
study needs it.

``` r
print(ds.omop.mutate("D_bmi", life_stage = case_when(
  is.na(age) ~ "unknown", age < 5 ~ "young child",
  age < 18 ~ "older child", .default = "adult"),
  newobj = "D_stage", conns = conns))
```

    #> [1] "D_stage"

``` r
stage_counts <- dsBaseClient::ds.table("D_stage$life_stage",
  useNA = "always", datasources = conns)
knitr::kable(book_counts(stage_counts))
```

| Category    | Count |
| :---------- | ----: |
| adult       |    14 |
| older child |     5 |
| young child |     9 |
| NA          |     0 |

The declared default applies only after the preceding conditions are
false. This recode distinguishes unknown age from a known age; it does
not impute age.

**Exercise:** recode both child labels to “no”, and the adult label to
“yes”.

Solution — recode a derived category

``` r
print(ds.omop.mutate("D_stage", adult = case_when(
  life_stage %in% c("young child", "older child") ~ "no",
  life_stage == "unknown" ~ NA,
  .default = "yes"), newobj = "D_recoded", conns = conns))
```

    #> [1] "D_recoded"

``` r
recode_counts <- dsBaseClient::ds.table("D_recoded$adult", useNA = "always",
  datasources = conns)
knitr::kable(book_counts(recode_counts))
```

| Category | Count |
| :------- | ----: |
| no       |    14 |
| yes      |    14 |
| NA       |     0 |

These counts agree with the earlier adult flag. Here the synthetic
source has complete ages, so the missingness branch introduces no extra
category count.

## Replace an object in place

Use the same name for `x` and `newobj` to replace that server object.
Omitting `newobj` instead generates a fresh name; it does not mean “in
place”.

``` r
print(ds.omop.mutate("D_bmi", height_m = height_cm / 100,
  bmi = weight_kg / (height_m * height_m), bmi_copy = bmi,
  newobj = "D_bmi", conns = conns))
```

    #> [1] "D_bmi"

``` r
before_columns <- dsBaseClient::ds.colnames("D_bmi", datasources = conns)
before_bmi <- dsBaseClient::ds.summary("D_bmi$bmi", datasources = conns)
c(copy_added = "bmi_copy" %in% before_columns$site,
  bmi_unchanged = identical(bmi_result, before_bmi))
```

    #>    copy_added bmi_unchanged 
    #>          TRUE          TRUE

The replacement adds `bmi_copy` while preserving BMI. The client keeps a
rollback copy until all participating sites succeed, restoring originals
if assignment fails. This single-site practice example verifies that
failure leaves the table usable; it does not simulate a network outage.

## Read the refusal messages

The following failures are intentional. First try an unknown function:

``` r
unknown_message <- tryCatch({
  ds.omop.mutate("D_bmi", bmi = sqrt(bmi), newobj = "D_bmi", conns = conns)
  "Unexpected success"
}, error = function(e) conditionMessage(e))
cat(unknown_message, "\n")
```

    #> ds.omop.mutate(): 'sqrt' is not part of the mutation grammar (+ - * /, conditions, ifelse, case_when, coalesce, cut, as.Date).

`sqrt` is outside the closed grammar and is refused by the client before
a request can alter the table. Use supported arithmetic or a documented
analysis function; arbitrary R functions cannot be sent through mutate.

Next try to copy a protected key. The server refuses this column
reference:

``` r
key_message <- tryCatch({
  ds.omop.mutate("D_bmi", copied = person_id,
    newobj = "D_key_copy", conns = conns)
  "Unexpected success"
}, error = function(e) conditionMessage(e))
cat(key_message, "\n")
```

    #> OMOP mutate failed and was rolled back: site: validation_error: omopMutateDS: column 'person_id' is a protected identifier and cannot be used in an expression; There are some DataSHIELD errors, list them with datashield.errors().

A class-1 source name is refused even when used as the name of a new
column:

``` r
blocked_message <- tryCatch({
  ds.omop.mutate("D_bmi", person_source_value = "label",
    newobj = "D_blocked_copy", conns = conns)
  "Unexpected success"
}, error = function(e) conditionMessage(e))
cat(blocked_message, "\n")
```

    #> OMOP mutate failed and was rolled back: site: validation_error: omopMutateDS: 'person_source_value' is a blocked (class-1) column name; choose another name; There are some DataSHIELD errors, list them with datashield.errors().

This is a blocked source/assignment name, not a request to alter its
protection. Use descriptive study-variable names for allowed derived
values.

Finally request an ordinary BMI change followed by a protected-key
overwrite in the same in-place call. The whole request must fail:

``` r
rollback_message <- tryCatch({
  ds.omop.mutate("D_bmi", bmi = bmi + 1, person_id = "replacement",
    newobj = "D_bmi", conns = conns)
  "Unexpected success"
}, error = function(e) conditionMessage(e))
cat(rollback_message, "\n")
```

    #> OMOP mutate failed and 'D_bmi' was left unchanged: site: validation_error: omopMutateDS: 'person_id' is a protected identifier and cannot be written; There are some DataSHIELD errors, list them with datashield.errors()

The protected-identifier message applies to person, episode and set keys
and any columns marked protected by the server. It prohibits both reads
and writes. Renaming a key is not an allowed transformation.

``` r
after_columns <- dsBaseClient::ds.colnames("D_bmi", datasources = conns)
after_bmi <- dsBaseClient::ds.summary("D_bmi$bmi", datasources = conns)
c(columns_preserved = identical(before_columns, after_columns),
  bmi_preserved = identical(before_bmi, after_bmi),
  refused_copies_absent = !any(c("D_key_copy", "D_blocked_copy") %in%
    DSI::datashield.symbols(conns)$site))
```

    #>     columns_preserved         bmi_preserved refused_copies_absent 
    #>                  TRUE                  TRUE                  TRUE

All checks are true. Even the earlier `bmi + 1` assignment was not
committed. These released checks establish schema and summary stability;
the book’s synthetic-source verification additionally checks equality of
the entire table.

## Filter using a predicate

A *predicate* is a condition. Only TRUE rows survive; missing conditions
are dropped. This retains adults without using a key to target a known
person.

``` r
print(ds.omop.filter("D_bmi", expr = omop_where(age >= 18),
  newobj = "D_adult_bmi", conns = conns))
```

    #> [1] "D_adult_bmi"

``` r
filtered_result <- dsBaseClient::ds.summary("D_adult_bmi$bmi", datasources = conns)
knitr::kable(book_summary(filtered_result))
```

| Site | Statistic |    Value |
| :--- | :-------- | -------: |
| site | 5%        | 22.75976 |
| site | 10%       | 23.69819 |
| site | 25%       | 25.78897 |
| site | 50%       | 28.03959 |
| site | 75%       | 29.79277 |
| site | 90%       | 30.36457 |
| site | 95%       | 30.45801 |
| site | Mean      | 27.34679 |

The subset meets the distinct-person release threshold in this practice
data. It remains a private table. A small or empty final subset can be
refused, as
[chapter 12](https://isglobal-brge.github.io/dsOMOPClient/articles/book-12-privacy.md)
demonstrates.

**Exercise:** keep the adult category from `D_stage` with the same
filter grammar.

Solution — filter a text category

``` r
print(ds.omop.filter("D_stage", expr = omop_where(life_stage == "adult"),
  newobj = "D_working_age", conns = conns))
```

    #> [1] "D_working_age"

``` r
working_result <- dsBaseClient::ds.summary("D_working_age$bmi", datasources = conns)
knitr::kable(book_summary(working_result))
```

| Site | Statistic |    Value |
| :--- | :-------- | -------: |
| site | 5%        | 22.75976 |
| site | 10%       | 23.69819 |
| site | 25%       | 25.78897 |
| site | 50%       | 28.03959 |
| site | 75%       | 29.79277 |
| site | 90%       | 30.36457 |
| site | 95%       | 30.45801 |
| site | Mean      | 27.34679 |

This category means ages 18 and over, so its summary agrees with the
preceding filter. The earlier missingness case ensures an unknown age is
not called adult.

## Try it

Bind the same template to inpatient visits and summarise the prior-visit
count. Do not alter the outpatient template in place.

Solution

``` r
inpatient <- recipe_bind(template, setting = 9201L)
book_symbols(recipe_execute(inpatient, out = c(baseline = "D_param_inpatient"), conns = conns))
```

    #>            baseline 
    #> "D_param_inpatient"

``` r
dsBaseClient::ds.summary("D_param_inpatient$prior_visits", datasources = conns)
```

    #> $site
    #> $site$class
    #> [1] "integer"
    #> 
    #> $site$length
    #> [1] 7
    #> 
    #> $site$`quantiles & mean`
    #>        5%       10%       25%       50%       75%       90%       95%      Mean 
    #>  2.000000  2.000000  2.500000  6.000000  8.500000 11.000000 12.500000  6.285714

Each binding is a separate recipe. Its own index changes which earlier
records fall into the lookback window. A numeric summary suits these
varied counts; a categorical table can be refused when it creates too
many distinct levels in a small population.

## Recap

Parameters change declared inputs, shortcuts express common native
recipes, and mutate/filter operate privately on assigned tables. Each
final object still passes publication checks.

**Next:** [12. What stays
private](https://isglobal-brge.github.io/dsOMOPClient/articles/book-12-privacy.md).
