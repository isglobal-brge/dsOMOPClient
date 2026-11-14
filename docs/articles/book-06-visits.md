# 13\. Visit dates, one idea at a time

**What you will learn**

Separate day offsets from calendar dates, select visit settings and
build one row per visit.

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

## Choose an anchor

An *anchor* is the date from which you measure time. Here it is the
public fixed date 2020-01-01. A day offset of zero means that date;
negative values mean earlier.

``` r
visit <- omop_event("visit_occurrence")
all_visits <- omop_window(NULL, NULL, anchor = "2020-01-01")
class(all_visits)
```

    #> [1] "omop_window" "list"

Both window ends are unbounded. The anchor defines the scale, not a
restriction to visits after January 1.

## First and last visit as day offsets

``` r
first <- omop_variable(event = visit,
  reducer = omop_reduce("first_date", window = all_visits))
last <- omop_variable(event = visit,
  reducer = omop_reduce("last_date", window = all_visits))
class(first)
```

    #> [1] "omop_variable" "list"

These reducers return integer days relative to the window anchor, not R
Date objects. People without visits retain missing values.

``` r
offsets <- omop_recipe(variables = list(first_days = first, last_days = last),
  outputs = omop_output("visits", format = "wide", grain = "person"))
stopifnot(recipe_explain(offsets, conns = conns)$selected == "v2")
print(recipe_execute(offsets, out = c(visits = "D_offsets"), conns = conns))
```

    #>      visits 
    #> "D_offsets"

``` r
dsBaseClient::ds.summary("D_offsets$first_days", datasources = conns)
```

    #> $site
    #> $site$class
    #> [1] "integer"
    #> 
    #> $site$length
    #> [1] 24
    #> 
    #> $site$`quantiles & mean`
    #>       5%      10%      25%      50%      75%      90%      95%     Mean 
    #>  73.0000  73.0000  73.0000 166.0000 201.0000 201.0000 201.0000 146.6667

The summary is an aggregate of private offsets. It is not a list of
visit dates.

``` r
dsBaseClient::ds.summary("D_offsets$last_days", datasources = conns)
```

    #> $site
    #> $site$class
    #> [1] "integer"
    #> 
    #> $site$length
    #> [1] 24
    #> 
    #> $site$`quantiles & mean`
    #>        5%       10%       25%       50%       75%       90%       95%      Mean 
    #>  201.0000  201.0000  201.0000  439.0000 1369.0000 1369.0000 1369.0000  669.6667

Some people have later visits. Taking a last date answers a different
question from taking the first; it does not describe duration of
observation.

## Use a shortcut with an indexed study

The shortcuts `omop_first_date()` and `omop_last_date()` use the study
index as their reference. An *index* selects the event that begins a
person’s study episode. Here each first visit is the index, so its
first-visit offset is zero.

``` r
visit_study <- omop_study(visit, outputs = "baseline",
  baseline = list(first_days = omop_first_date("visit_occurrence"),
                  last_days = omop_last_date("visit_occurrence")))
stopifnot(recipe_explain(visit_study, conns = conns)$selected == "v2")
print(recipe_execute(visit_study, out = c(baseline = "D_visit_study"), conns = conns))
```

    #>        baseline 
    #> "D_visit_study"

``` r
dsBaseClient::ds.summary("D_visit_study$first_days", datasources = conns)
```

    #> $site
    #> $site$class
    #> [1] "integer"
    #> 
    #> $site$length
    #> [1] 18
    #> 
    #> $site$`quantiles & mean`
    #>   5%  10%  25%  50%  75%  90%  95% Mean 
    #>    0    0    0    0    0    0    0    0

This is the simple layer of the same language-2 API. It expands into an
ordinary recipe. It includes people with an eligible index visit, so
unlike our all-person wide table it excludes the people who never
visited.

## Assign an actual calendar date

``` r
calendar <- omop_recipe(variables = list(visit_date = omop_date("first_visit")),
  outputs = omop_output("visits", format = "wide", grain = "person"))
stopifnot(recipe_explain(calendar, conns = conns)$selected == "v2")
print(recipe_execute(calendar, out = c(visits = "D_calendar"), conns = conns))
```

    #>       visits 
    #> "D_calendar"

`omop_date()` defaults to day precision and assigns a Date on the
server. An exact date is a class-2 quasi-identifier: a value that can
help identify someone when combined with other information. The released
assign path permits it without an owner-side absolute-date switch. The
analyst still cannot fetch its rows.

For a useful categorical aggregate, explicitly choose month precision:

``` r
months <- omop_recipe(variables = list(visit_month = omop_date("first_visit",
  omop_date_spec(precision = "month", level_range = c("2020-03-01", "2020-07-31")))),
  outputs = omop_output("visits", format = "wide", grain = "person"))
stopifnot(recipe_explain(months, conns = conns)$selected == "v2")
print(recipe_execute(months, out = c(visits = "D_months"), conns = conns))
```

    #>     visits 
    #> "D_months"

``` r
ds.omop.table("D_months", "visit_month", conns = conns)
```

    #> D_months$visit_month
    #> 2020-03 2020-04 2020-05 2020-06 2020-07 
    #>       6       0       0       6       6

The public range declares month categories, including empty months. It
is a study definition, not a range discovered by requesting minimum and
maximum patient dates. The factor labels use year-month order so that
they sort chronologically.

## Filter inpatient or outpatient visits

In OMOP, these settings are concepts in `visit_concept_id`. The separate
`visit_type_concept_id` describes provenance. Inspect the settings by
name:

``` r
settings <- ds.omop.concept.lookup(c(9201L, 9202L), conns = conns)
settings$pooled["concept_name"]
```

    #>       concept_name
    #> 1  Inpatient Visit
    #> 2 Outpatient Visit

``` r
inpatient <- omop_event("visit_occurrence", concepts = 9201L)
setting <- omop_variable(event = inpatient,
  reducer = omop_reduce("first_value", value = "visit_concept_id"))
class(setting)
```

    #> [1] "omop_variable" "list"

The event filter keeps inpatient records; the reducer reads the setting
of the first such record. People without inpatient records have a
missing setting.

``` r
inpatient_people <- omop_recipe(variables = list(setting = setting),
  outputs = omop_output("people", format = "wide", grain = "person"))
stopifnot(recipe_explain(inpatient_people, conns = conns)$selected == "v2")
print(recipe_execute(inpatient_people, out = c(people = "D_inpatient"), conns = conns))
```

    #>        people 
    #> "D_inpatient"

``` r
ds.omop.table("D_inpatient", "setting", conns = conns)
```

    #> D_inpatient$setting
    #> Inpatient Visit 
    #>              12

This counts people with an inpatient setting, not total inpatient
visits.

## Keep one row per visit

A *long* output retains event rows. Register an event named `visits` and
ask for its records. Long outputs normalise the source date to
`start_date` and the setting to `concept_id`. Day precision keeps actual
calendar dates:

``` r
long_visits <- omop_recipe(
  events = list(visits = omop_event("visit_occurrence", concepts = c(9201L, 9202L), name = "visits")),
  outputs = omop_output("visits", format = "long", sources = "visits"),
  date_spec = omop_date_spec(precision = "day"))
stopifnot(recipe_explain(long_visits, conns = conns)$selected == "v2")
print(recipe_execute(long_visits, out = c(visits = "D_visits"), conns = conns))
```

    #>     visits 
    #> "D_visits"

``` r
dsBaseClient::ds.colnames("D_visits", datasources = conns)
```

    #> $site
    #> [1] "row_id"                "cohort_row_id"         "person_id"            
    #> [4] "concept_id"            "start_date"            "end_date"             
    #> [7] "visit_type_concept_id"

The column inventory describes a private table. It does not return a
visit row. The source’s `visit_start_date` is now called `start_date`.
If your analysis needs the source spelling, add a value-preserving alias
on the server:

``` r
print(ds.omop.mutate("D_visits", visit_start_date = start_date,
                     newobj = "D_visit_dates", conns = conns))
```

    #> [1] "D_visit_dates"

``` r
dsBaseClient::ds.colnames("D_visit_dates", datasources = conns)
```

    #> $site
    #> [1] "row_id"                "cohort_row_id"         "person_id"            
    #> [4] "concept_id"            "start_date"            "end_date"             
    #> [7] "visit_type_concept_id" "visit_start_date"

This copies the date column inside the protected table. It preserves one
row per visit and returns only the new symbol name and column names.

``` r
dsBaseClient::ds.table("D_visits$concept_id", useNA = "no", datasources = conns)
```

    #> $output.list
    #> $output.list$TABLE_rvar.by.study_row.props
    #>                    study
    #> D_visits$concept_id site
    #>    Inpatient Visit     1
    #>    Outpatient Visit    1
    #> 
    #> $output.list$TABLE_rvar.by.study_col.props
    #>                    study
    #> D_visits$concept_id site
    #>    Inpatient Visit   0.4
    #>    Outpatient Visit  0.6
    #> 
    #> $output.list$TABLE_rvar.by.study_counts
    #>                    study
    #> D_visits$concept_id site
    #>    Inpatient Visit    12
    #>    Outpatient Visit   18
    #> 
    #> $output.list$TABLES.COMBINED_all.sources_proportions
    #> D_visits$concept_id
    #>  Inpatient Visit Outpatient Visit 
    #>              0.4              0.6 
    #> 
    #> $output.list$TABLES.COMBINED_all.sources_counts
    #> D_visits$concept_id
    #>  Inpatient Visit Outpatient Visit 
    #>               12               18 
    #> 
    #> 
    #> $validity.message
    #> [1] "Data in all studies were valid"

These are counts of visit records by translated setting. A person can
contribute several visits, so do not call these counts numbers of
distinct people.

## Recap

Choose offsets for elapsed time and omop\_date for calendar
presentation. Wide selects one value per person; long keeps visit rows
privately.

## Try it

Use a typed parameter to make the setting recipe reusable, bind it to
outpatient visits, and count the translated setting.

Solution

``` r
setting_id <- omop_param("setting", type = "concept_set")
template <- omop_recipe(variables = list(setting = omop_variable(
  event = omop_event("visit_occurrence", concepts = setting_id),
  reducer = omop_reduce("first_value", value = "visit_concept_id"))),
  outputs = omop_output("people", format = "wide", grain = "person"))
outpatient <- recipe_bind(template, setting = 9202L)
stopifnot(recipe_explain(outpatient, conns = conns)$selected == "v2")
print(recipe_execute(outpatient, out = c(people = "D_outpatient"), conns = conns))
```

    #>         people 
    #> "D_outpatient"

``` r
ds.omop.table("D_outpatient", "setting", conns = conns)
```

    #> D_outpatient$setting
    #> Outpatient Visit 
    #>               18
