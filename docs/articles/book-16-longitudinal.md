# 16\. Longitudinal data: from events to analysis

**What you will learn**

Choose wide, long or person-period data; handle irregular measurements
and missing periods; reshape privately; and fit a discrete-time hazard
model and repeated-measure mixed models across two connections.

**You need:** [Before you
start](https://isglobal-brge.github.io/dsOMOPClient/articles/book-00-start.html#before-you-start),
[output
formats](https://isglobal-brge.github.io/dsOMOPClient/articles/book-08-outputs.md)
and [lab
values](https://isglobal-brge.github.io/dsOMOPClient/articles/book-14-labs.md).
Start a fresh session in `book-work` and run every section in order,
including solutions. GiBleed and Synthea are public **synthetic**
resources. Their results teach analysis mechanics, not clinical
conclusions. This chapter uses dsBaseClient 6.3.5.

Concept selections refer to the searches and saved definitions in
[chapter 4](https://isglobal-brge.github.io/dsOMOPClient/articles/book-04-vocabulary.md),
including the separate questionnaire catalogue. Additional
body-measurement concepts are found on screen in the [formula
walkthrough](https://isglobal-brge.github.io/dsOMOPClient/articles/book-15-formulas.html#find-the-inputs-before-calculating).

## The same people, two shapes

*Longitudinal* means following the same people over time. Here is a tiny
invented picture, **not a DataSHIELD row release**. The same three
fictional people have these measurements; days are relative to each
person’s entry.

| Person | Value on day 0 | Value on day 30 |
| ------ | -------------: | --------------: |
| A      |             14 |              15 |
| B      |             13 |              NA |
| C      |             15 |              14 |

Turning the two measurement columns into rows gives this long picture:

| Person | Day | Value |
| ------ | --: | ----: |
| A      |   0 |    14 |
| A      |  30 |    15 |
| B      |   0 |    13 |
| B      |  30 |    NA |
| C      |   0 |    15 |
| C      |  30 |    14 |

This is a scheduled grid: B’s second row explicitly represents a missing
value. An event-long table would omit B’s day-30 row if no event
occurred. Its absence must not be interpreted as a measured zero. Six
rows still represent three people.

**Exercise.** Which shape would you use to compare baseline means? Which
for a model of change over time?

Solution

Use the wide day-0 column for baseline means. Use long data with a
person key and time for repeated-measure models. Account for dependence
between the same person’s rows. Taking an ordinary mean over long rows
weights frequent attenders more.

## Choose the shape before requesting it

| Question                                         | Shape          | What a row means                                |
| ------------------------------------------------ | -------------- | ----------------------------------------------- |
| Baseline lab or cross-sectional comparison       | wide           | One person or entry episode                     |
| How do repeated readings vary with time?         | long           | One observed measurement, with time since index |
| Did an outcome occur during this at-risk period? | person\_period | One person-episode and time bin                 |
| How long until the first outcome or censoring?   | survival       | One follow-up record                            |
| When was an exposure active?                     | intervals      | One exposure start and end                      |

Time-varying covariates belong to periods: use information available by
that period’s start. A future lab mean would leak future information
into a hazard model. A survival output must specify censoring. Intervals
retain both ends of an exposure; they are not automatically
nonoverlapping. Chapter 8 executes all five shapes.

Start with GiBleed and request visit rows directly from a recipe.

``` r
library(dsOMOPClient)
source("tools/book/display.R")
source("tools/book/connection.R")
source("tools/book/practice.R")
practice <- book_connect("gibleed")
conns <- practice$conns
visits <- omop_event("visit_occurrence", concepts = 9201L, name = "admission")
visitors <- omop_population("visitors", index = omop_index(visits))
visit_recipe <- omop_recipe(events = list(visits), populations = list(visitors),
  outputs = omop_output("visits", rows = "visitors", format = "long", sources = "admission",
    options = list(columns_derived = c("days_from_index", "duration_days"))))
book_symbols(recipe_execute(visit_recipe, out = c(visits = "D_visit_long"), conns = conns))
```

    #>         visits 
    #> "D_visit_long"

The long table remains private. Release only the named visit-type counts
and column names, which describe the representation.

``` r
dsBaseClient::ds.colnames("D_visit_long", datasources = conns)
```

    #> $site
    #>  [1] "row_id"                  "cohort_row_id"           "person_id"              
    #>  [4] "concept_id"              "start_date"              "end_date"               
    #>  [7] "visit_type_concept_id"   "discharge_to_concept_id" "days_from_index"        
    #> [10] "duration_days"

``` r
visit_counts <- dsBaseClient::ds.table("D_visit_long$concept_id", datasources = conns)
knitr::kable(book_counts(visit_counts))
```

| Category        | Count |
| :-------------- | ----: |
| Inpatient Visit |  1037 |
| NA              |     0 |

**Exercise.** Summarise the time since entry. Does a long format
guarantee repeated visits?

Solution

``` r
visit_time <- dsBaseClient::ds.mean("D_visit_long$days_from_index", type = "combine", datasources = conns)
visit_time
```

    #> $Global.Mean
    #>                 EstimatedMean Nmissing Nvalid Ntotal
    #> studiesCombined      1272.554        0   1037   1037
    #> 
    #> $Nstudies
    #> [1] 1
    #> 
    #> $ValidityMessage
    #>      ValidityMessage 
    #> site "VALID ANALYSIS"

No. Here the mean is about 1273 days: the first admission contributes
zero, and subsequent admissions contribute positive offsets. Some people
still have only one admission. This is an event-weighted mean, not the
mean gap between admissions. Switch to Synthea for repeated numeric
measurements.

## Irregular measurements: define windows first

``` r
ds.omop.disconnect(conns = conns)
DSI::datashield.logout(conns)
practice <- book_connect("numeric")
conns <- practice$conns
lab <- omop_event("measurement", concepts = 3000963L, value = "value_as_number",
  where = omop_where(unit_concept_id == 8713L), name = "hb")
entry <- omop_population("entry", index = omop_index(lab))
class(entry)
```

    #> [1] "omop_population" "list"

Index is each person’s first hemoglobin. These synthetic records are
irregularly spaced over years. Use three **1000-day windows**, not
calendar years. Window 1 includes days 0–999, window 2 days 1000–1999,
and window 3 days 2000–2999. First and last refer to actual dates within
each window, not scheduled visits.

``` r
window_value <- function(which, start, end) {
  omop_variable(event = lab, reducer = omop_reduce(which,
    window = omop_window(start, end)))
}
windows <- omop_recipe(events = list(lab), populations = list(entry), variables = list(
  first1 = window_value("first_value", 0, 999),
  first2 = window_value("first_value", 1000, 1999),
  first3 = window_value("first_value", 2000, 2999),
  last1 = window_value("last_value", 0, 999),
  last2 = window_value("last_value", 1000, 1999),
  last3 = window_value("last_value", 2000, 2999)),
  outputs = omop_output("wide", rows = "entry", format = "wide"))
book_symbols(recipe_execute(windows, out = c(wide = "D_windows"), conns = conns))
```

    #>        wide 
    #> "D_windows"

No eligible nonmissing value gives `NA`. It never gives zero hemoglobin.
Before analysing a period, count its missing values and summarise its
first and last readings. The following loop repeats the same three
explicit aggregate calls.

``` r
window_results <- lapply(1:3, function(period) {
  first_name <- paste0("D_windows$first", period)
  last_name <- paste0("D_windows$last", period)
  first_mean <- dsBaseClient::ds.mean(first_name, type = "combine", datasources = conns)
  last_mean <- dsBaseClient::ds.mean(last_name, type = "combine", datasources = conns)
  data.frame(Period = period,
    First = first_mean$Global.Mean[1, 1], Last = last_mean$Global.Mean[1, 1],
    Observed = first_mean$Global.Mean[1, "Nvalid"], Missing = first_mean$Global.Mean[1, "Nmissing"])
})
knitr::kable(do.call(rbind, window_results))
```

| Period |    First |     Last | Observed | Missing |
| -----: | -------: | -------: | -------: | ------: |
|      1 | 13.98571 | 14.28571 |       28 |       0 |
|      2 | 15.30000 | 15.30000 |       10 |      18 |
|      3 | 14.56667 | 14.56667 |       15 |      13 |

These means use the observed people in each period; changing means can
therefore reflect changing membership. A missing measurement does not
prove that the person left observation. Check observation coverage
separately for a follow-up analysis.

**Exercise.** Make missingness a named category for each period and
release its counts.

Solution

``` r
print(ds.omop.mutate("D_windows",
  missing1 = ifelse(is.na(first1), "missing", "observed"),
  missing2 = ifelse(is.na(first2), "missing", "observed"),
  missing3 = ifelse(is.na(first3), "missing", "observed"),
  newobj = "D_missing", conns = conns))
```

    #> [1] "D_missing"

``` r
missing_counts <- lapply(1:3, function(period) {
  result <- dsBaseClient::ds.table(paste0("D_missing$missing", period), datasources = conns)
  transform(book_counts(result), Period = period)
})
knitr::kable(do.call(rbind, missing_counts))
```

| Category | Count | Period |
| :------- | ----: | -----: |
| observed |    28 |      1 |
| NA       |     0 |      1 |
| missing  |    18 |      2 |
| observed |    10 |      2 |
| NA       |     0 |      2 |
| missing  |    13 |      3 |
| observed |    15 |      3 |
| NA       |     0 |      3 |

The source checks find 28, 10 and 15 observed people: therefore 0, 18
and 13 missing values. Do not carry a measurement forward without
declaring how long it remains valid and what that assumption means.

## Wide columns at prespecified time points

A wide table can hold several scheduled values for each person. A named
grid makes the target times, tolerances and selection rule explicit.
Each value comes from an eligible observed event; no measurement is
interpolated.

``` r
points <- omop_timepoints(list(
  baseline = omop_timepoint(),
  year3 = omop_timepoint(3, "years", tolerance = c(-365, 365))),
  anchor = "index", reuse = FALSE)
repeated_hb <- omop_repeated(lab, values = list(hb = omop_number_rows(lab)),
  timepoints = points, missing = "keep")
repeated_recipe <- omop_recipe(events = list(lab), populations = list(entry),
  variables = list(measurements = repeated_hb),
  outputs = omop_output("wide", rows = "entry", format = "wide"))
repeated_result <- recipe_execute(repeated_recipe, out = "D_timepoints", conns = conns)
dsBaseClient::ds.colnames("D_timepoints", datasources = conns)
```

    #> $site
    #> [1] "row_id"                   "cohort_row_id"            "person_id"               
    #> [4] "measurements_hb_baseline" "measurements_hb_year3"

The generated names retain the measurement and time-point names.
`closest` is the default selection; `first` or `last` would choose
differently within the inclusive tolerance. `reuse = FALSE` prevents an
event from filling two points. The tolerance here is a teaching choice
for irregular data, not a claim that a lab a year away represents a
clinical three-year visit.

**Exercise.** Inspect the baseline column’s released summary.

Solution: a scheduled column

``` r
knitr::kable(book_summary(dsBaseClient::ds.summary(
  "D_timepoints$measurements_hb_baseline", datasources = conns)))
```

| Site | Statistic |    Value |
| :--- | :-------- | -------: |
| site | 5%        | 11.29000 |
| site | 10%       | 12.48000 |
| site | 25%       | 12.97500 |
| site | 50%       | 13.70000 |
| site | 75%       | 15.42500 |
| site | 90%       | 16.19000 |
| site | 95%       | 16.85500 |
| site | Mean      | 13.98571 |

The baseline target is day zero at the first lab. Later targets without
an eligible reading remain missing; they are not zero-valued
measurements.

**Exercise.** Where can you check the expanded columns’ types before
modelling?

Solution: the published table codebook

``` r
knitr::kable(recipe_codebook(repeated_result)[[1]]$columns[c("name", "r_type", "unit")])
```

| name                       | r\_type   | unit               |
| :------------------------- | :-------- | :----------------- |
| row\_id                    | character | not\_applicable    |
| cohort\_row\_id            | character | not\_applicable    |
| person\_id                 | character | not\_applicable    |
| measurements\_hb\_baseline | double    | unresolved\_source |
| measurements\_hb\_year3    | double    | unresolved\_source |

Use the numeric columns in a model only after choosing how to handle
missing scheduled readings. A codebook does not expose their missingness
frequency.

## Ask the recipe, or reshape on the server

The installed `ds.reShape()` can turn existing wide columns into period
rows. The native recipe uses names without dots. Create three private
aliases with the `first.1`, `first.2`, `first.3` names expected by this
dsBase reshape method. This operation assigns privately; it never
returns individual rows.

``` r
stopifnot(all(c("ds.reShape", "ds.lmerSLMA", "ds.glmerSLMA") %in%
                getNamespaceExports("dsBaseClient")))
for (period in 1:3) {
  dsBaseClient::ds.asNumeric(x.name = paste0("D_windows$first", period),
    newobj = paste0("first.", period), datasources = conns)
}
dsBaseClient::ds.dataFrame(c("D_windows$person_id", "first.1", "first.2", "first.3"),
  newobj = "D_for_reshape", datasources = conns)
```

    #> $is.object.created
    #> [1] "A data object <D_for_reshape> has been created in all specified data sources"
    #> 
    #> $validity.check
    #> [1] "<D_for_reshape> appears valid in all sources"

``` r
dsBaseClient::ds.reShape("D_for_reshape", varying = c("first.1", "first.2", "first.3"),
  timevar.name = "period", idvar.name = "person_id", direction = "long",
  newobj = "D_reshaped", datasources = conns)
```

    #> $is.object.created
    #> [1] "A data object <D_reshaped> has been created in all specified data sources"
    #> 
    #> $validity.check
    #> [1] "<D_reshaped> appears valid in all sources"

``` r
dsBaseClient::ds.colnames("D_reshaped", datasources = conns)
```

    #> $site
    #> [1] "person_id" "period"    "first"

This gives each person three period rows, including missing first
values. Only the person key and three first values enter this reshape;
last values are not copied into every period. Its `period` is 1, 2 or 3,
**not an actual measurement date**. A reshape cannot recover dates or
readings discarded by a reducer. `ds.reShape` also depends on the
server’s allowed privacy profile; this run uses the installed practice
configuration.

For a dense panel with last values and event counts, ask the recipe
directly. The native panel supports `last` and `mean`, but not a `first`
analysis. The wide window reducers above provide first values explicitly
without inventing a panel option.

``` r
panel <- omop_recipe(events = list(lab), populations = list(entry),
  outputs = omop_output("panel", rows = "entry", format = "person_period",
    options = list(sources = list(lab), bins = list(width = 1000L),
      window = omop_window(0, 2999), analyses = c("count", "last"), representation = "dense")))
book_symbols(recipe_execute(panel, out = c(panel = "D_panel"), conns = conns))
```

    #>     panel 
    #> "D_panel"

``` r
dsBaseClient::ds.colnames("D_panel", datasources = conns)
```

    #> $site
    #> [1] "row_id"            "cohort_row_id"     "person_id"         "period_start_days"
    #> [5] "period_end_days"   "days"              "hb_count"          "hb_last"

The panel respects observation coverage and retains empty observed bins:
a zero count and a missing last value describe different things. It can
omit time outside coverage, so it need not have the same roster as the
wide-window reshape. Requesting the right shape keeps keys, windows,
coverage and event semantics in one checked plan.

**Exercise.** Verify that the reshape kept three rows per person,
without reading rows.

Solution

``` r
period_counts <- dsBaseClient::ds.table("D_reshaped$period", datasources = conns)
knitr::kable(book_counts(period_counts))
```

| Category | Count |
| :------- | ----: |
| 1        |    28 |
| 2        |    28 |
| 3        |    28 |
| NA       |     0 |

There are 28 rows in each period, including rows with missing
measurements. That is 84 rows, still 28 people. A row-count increase is
not a sample-size increase.

## Build a risk set for a discrete-time hazard

A *hazard* is the probability of an outcome in the current period among
people still at risk at its start. Here the outcome is the **next
recorded hemoglobin**, not illness or death. Start on day 1 after the
first reading, and stop at the first subsequent reading, death,
observation end or day 7300, whichever comes first. The absence of a
repeat lab is not a normal result.

For this section use the same disjoint public Synthea split as chapter
15: 14 people at each of two local DSLite servers. No people are
duplicated. A real multisite study must establish nonoverlapping
populations separately.

``` r
pair <- book_numeric_pair()
two_conns <- pair$conns
names(two_conns)
```

    #> [1] "north" "south"

The panel already stops at observation coverage. Attach one
first-outcome day and one death day per episode, using a checked
many-to-one join. These dates are index-relative numbers and stay
private.

``` r
death <- omop_event("death", name = "death")
hazard <- omop_recipe(events = list(lab, death), populations = list(entry), variables = list(
  next_day = omop_variable(event = lab,
    reducer = omop_reduce("first_date", window = omop_window(1, NULL))),
  death_day = omop_variable(event = death, reducer = omop_reduce("first_date"))),
  outputs = list(
    omop_output("daily", rows = "entry", format = "person_period",
      options = list(sources = list(lab), bins = list(width = 1L),
        window = omop_window(1, 7300), analyses = "count", representation = "dense")),
    omop_output("ends", rows = "entry", format = "wide"),
    omop_output("risk", rows = "entry", format = "joined",
      options = list(from = list(list(output = "daily"), list(output = "ends")),
        how = "left", cardinality = "many_to_one"))))
hazard$publish <- "risk"
book_symbols(recipe_execute(hazard, out = c(risk = "D_days"), conns = two_conns, symbol = "book_pool"))
```

    #>     risk 
    #> "D_days"

Remove days after the first repeat or death. A missing next-day value
means “no subsequent lab found”, so retain observed days until another
censor applies. We count an event on the death day if both dates
coincide; state this tie rule.

``` r
print(ds.omop.mutate("D_days", to_next = next_day - period_start_days,
  to_death = death_day - period_start_days,
  newobj = "D_remaining", conns = two_conns, symbol = "book_pool"))
```

    #> [1] "D_remaining"

``` r
print(ds.omop.filter("D_remaining",
  expr = omop_where((is.na(to_next) | to_next >= 0) &
  (is.na(to_death) | to_death >= 0)),
  newobj = "D_risk", conns = two_conns, symbol = "book_pool"))
```

    #> [1] "D_risk"

``` r
print(ds.omop.mutate("D_risk", event = ifelse(hb_count > 0, 1L, 0L),
  years = period_start_days / 365.25,
  newobj = "D_hazard", conns = two_conns, symbol = "book_pool"))
```

    #> [1] "D_hazard"

``` r
hazard_counts <- dsBaseClient::ds.table("D_hazard$event", datasources = two_conns)
knitr::kable(book_counts(hazard_counts))
```

| Category | Count |
| :------- | ----: |
| 0        | 48535 |
| 1        |    25 |
| NA       |     0 |

Each row is one observed day at risk. More than one lab on that day is
still one outcome. Time in years is a predictor; the outcome probability
remains **per day**. The binomial GLM models log odds with a linear time
trend. A study would assess that time specification and the assumption
of noninformative censoring.

The dsBase 6.3.5 local setup fits use one iteration and emit the
explained `glm.fit: algorithm did not converge` warnings. The
distributed fit then iterates; inspect its final diagnostics, which the
book checks against a central fit of the public source. The warnings
below are retained, not treated as evidence of success.

``` r
hazard_fit <- dsBaseClient::ds.glm("event ~ years", data = "D_hazard",
  family = "binomial", datasources = two_conns)
```

    #> Warning: glm.fit: algorithm did not converge
    #> Warning: glm.fit: algorithm did not converge
    #> Warning: glm.fit: algorithm did not converge
    #> Warning: glm.fit: algorithm did not converge
    #> Warning: glm.fit: algorithm did not converge
    #> Warning: glm.fit: algorithm did not converge
    #> Warning: glm.fit: algorithm did not converge
    #> Warning: glm.fit: algorithm did not converge
    #> Warning: glm.fit: algorithm did not converge
    #> Warning: glm.fit: algorithm did not converge
    #> Warning: glm.fit: algorithm did not converge
    #> Warning: glm.fit: algorithm did not converge
    #> Warning: glm.fit: algorithm did not converge
    #> Warning: glm.fit: algorithm did not converge
    #> Warning: glm.fit: algorithm did not converge
    #> Warning: glm.fit: algorithm did not converge
    #> Warning: glm.fit: algorithm did not converge
    #> Warning: glm.fit: algorithm did not converge
    #> Warning: glm.fit: algorithm did not converge
    #> Warning: glm.fit: algorithm did not converge
    #> Warning: glm.fit: algorithm did not converge
    #> Warning: glm.fit: algorithm did not converge
    #> Warning: glm.fit: algorithm did not converge
    #> Warning: glm.fit: algorithm did not converge
    #> Warning: glm.fit: algorithm did not converge
    #> Warning: glm.fit: algorithm did not converge
    #> Warning: glm.fit: algorithm did not converge
    #> Warning: glm.fit: algorithm did not converge

``` r
hazard_table <- as.data.frame(hazard_fit$coefficients[, 1:4, drop = FALSE])
hazard_table[["p-value"]] <- format.pval(hazard_table[["p-value"]], digits = 3, eps = 1e-16)
knitr::kable(hazard_table, digits = 4)
```

|             |  Estimate | Std. Error |   z-value | p-value  |
| :---------- | --------: | ---------: | --------: | :------- |
| (Intercept) | \-12.9553 |     1.2056 | \-10.7463 | \< 1e-16 |
| years       |    1.2945 |     0.2344 |    5.5221 | 3.35e-08 |

``` r
list(iterations = hazard_fit$iter, valid_days = hazard_fit$Nvalid,
     disclosure_risk = hazard_fit$disclosure.risk, errors = hazard_fit$errorMessage)
```

    #> $iterations
    #> [1] 13
    #> 
    #> $valid_days
    #> [1] 48560
    #> 
    #> $disclosure_risk
    #>       RISK OF DISCLOSURE
    #> north                  0
    #> south                  0
    #> 
    #> $errors
    #>       ERROR MESSAGES
    #> north "No errors"   
    #> south "No errors"

This is a jointly estimated distributed GLM. Its risk-set likelihood has
one first event at most per person; it is not a logistic model of every
recurrent reading treated as an independent patient.

**Exercise.** Show the fitted odds ratio per additional year since
entry. Does it mean a yearly probability of getting a lab?

Solution

``` r
exp(hazard_fit$coefficients["years", 1])
```

    #> [1] 3.649034

No. It compares the odds of a repeat **on the current day**, conditional
on having remained at risk, for a one-year difference in time since
entry. It is not a causal effect or an annual risk. The event is
recorded care, not disease onset.

## Repeated measures and mixed models

For actual repeated-value modelling use Synthea’s BMI readings, whose
within-person trajectories provide a more useful random-intercept
example than hemoglobin here. A *random intercept* lets people have
different baseline levels and accounts for correlation between their
repeated readings. This is a different question from time to the first
repeat lab.

``` r
bmi <- omop_event("measurement", concepts = 3038553L, value = "value_as_number",
  where = omop_where(unit_concept_id == 9531L), name = "bmi")
bmi_entry <- omop_population("bmi_entry", index = omop_index(bmi))
repeated <- omop_recipe(events = list(bmi), populations = list(bmi_entry),
  outputs = omop_output("long", rows = "bmi_entry", format = "long", sources = "bmi",
    options = list(columns_derived = c("value", "unit", "days_since_previous"))))
book_symbols(recipe_execute(repeated, out = c(long = "D_long"), conns = two_conns, symbol = "book_pool"))
```

    #>     long 
    #> "D_long"

``` r
dsBaseClient::ds.colnames("D_long", datasources = two_conns)
```

    #> $north
    #>  [1] "row_id"                      "cohort_row_id"              
    #>  [3] "person_id"                   "concept_id"                 
    #>  [5] "start_date"                  "end_date"                   
    #>  [7] "measurement_type_concept_id" "value_as_concept_id"        
    #>  [9] "meas_event_field_concept_id" "days_since_previous"        
    #> [11] "value"                       "unit_concept_id"            
    #> 
    #> $south
    #>  [1] "row_id"                      "cohort_row_id"              
    #>  [3] "person_id"                   "concept_id"                 
    #>  [5] "start_date"                  "end_date"                   
    #>  [7] "measurement_type_concept_id" "value_as_concept_id"        
    #>  [9] "meas_event_field_concept_id" "days_since_previous"        
    #> [11] "value"                       "unit_concept_id"

`start_date` is days since first BMI under the default relative-date
policy. The person key, all readings and their dates stay on the
servers. Preserve actual time; coding visit numbers as equally spaced
would misrepresent these records. Each person has exactly one indexed
episode here, so `cohort_row_id` is the person-level grouping key for
these models. Using this stable episode key also keeps the local
teaching runs reproducible when temporary resource paths change
pseudonym labels. With multiple episodes per person, use a person-level
key to model their dependence instead.

``` r
print(ds.omop.mutate("D_long", years = start_date / 365.25,
  above_cutoff = ifelse(value >= 25, 1L, 0L),
  newobj = "D_repeat", conns = two_conns, symbol = "book_pool"))
```

    #> [1] "D_repeat"

``` r
repeat_mean <- dsBaseClient::ds.mean("D_repeat$value", type = "combine", datasources = two_conns)
repeat_counts <- dsBaseClient::ds.table("D_repeat$above_cutoff", datasources = two_conns)
knitr::kable(book_group_comparison(repeat_counts, repeat_mean, "BMI reading", "kg/m²"))
```

| Category | Count | Number      | Unit  |     Mean |
| :------- | ----: | :---------- | :---- | -------: |
| 0        |   123 | BMI reading | kg/m² | 25.76941 |
| 1        |   315 | BMI reading | kg/m² | 25.76941 |
| NA       |     0 | BMI reading | kg/m² | 25.76941 |

This mean weights readings, not people. The cutoff is a teaching rule
only: this synthetic population includes children, and an adult BMI
cutoff is not an age-specific clinical classification. The continuous
and binary responses answer different questions.

`SLMA` means **study-level meta-analysis**. Each server fits its own
mixed model; only estimates and standard errors return for pooling. It
is not a single mixed model fitted jointly to concatenated rows. Here we
show fixed-effect meta-analysis columns (`pooled.FE`, `se.FE`); this
assumes a common coefficient across sites. The method also returns
ML/REML random-effects pooling, with little information about
between-site heterogeneity from only two sites.

The installed dsBase calls `lme4::nobars`, which in this dependency
version emits a deprecation warning about moving to `reformulas`. It
concerns the upstream API, not the data. We retain and explain that
exact warning; model convergence and singularity are checked separately
rather than suppressing diagnostics.

``` r
linear_mixed <- dsBaseClient::ds.lmerSLMA("value ~ years + (1|cohort_row_id)",
  dataName = "D_repeat", datasources = two_conns)
```

    #> Warning: the 'nobars' function has moved to the reformulas package. Please update your
    #> imports, or ask an upstream package maintainer to do so.

``` r
knitr::kable(linear_mixed$SLMA.pooled.ests.matrix[, c("pooled.FE", "se.FE"), drop = FALSE])
```

|             |  pooled.FE |     se.FE |
| :---------- | ---------: | --------: |
| (Intercept) | 23.3504584 | 0.9636351 |
| years       |  0.1507029 | 0.0153574 |

``` r
linear_mixed$Convergence.error.message
```

    #> [1] "Study1: no convergence error reported" "Study2: no convergence error reported"

The time coefficient is BMI units per year under this linear trajectory
model. Real growth may be nonlinear; these data do not validate a
constant-slope assumption. No individual random intercepts are
downloaded.

``` r
binary_mixed <- dsBaseClient::ds.glmerSLMA("above_cutoff ~ years + (1|cohort_row_id)",
  family = "binomial", dataName = "D_repeat", datasources = two_conns)
knitr::kable(binary_mixed$SLMA.pooled.ests.matrix[, c("pooled.FE", "se.FE"), drop = FALSE])
```

|             | pooled.FE |     se.FE |
| :---------- | --------: | --------: |
| (Intercept) | 4.7059447 | 2.6100909 |
| years       | 0.0093367 | 0.0526681 |

``` r
binary_mixed$Convergence.error.message
```

    #> [1] "Study1: no convergence error reported" "Study2: no convergence error reported"

This coefficient is on the log-odds scale for a reading above the
teaching cutoff, conditional on the person’s random intercept. It is
neither a BMI-unit slope nor the first-event hazard from the preceding
section.

**Exercise 1.** Show the number of contributing sites and the
binary-model time odds ratio. Is SLMA pooling equivalent to the
preceding distributed GLM?

Solution 1

``` r
list(sites = binary_mixed$num.valid.studies,
     time_odds_ratio = exp(binary_mixed$SLMA.pooled.ests.matrix["years", "pooled.FE"]))
```

    #> $sites
    #> [1] 2
    #> 
    #> $time_odds_ratio
    #> [1] 1.00938

No. SLMA combines site coefficients and their uncertainty. The
distributed GLM combines iteration statistics to estimate one model.
Both need compatible variable meaning and disjoint populations; they
solve different estimation problems.

**Exercise 2.** Would a singular fit be proof that a mixed model worked
well?

Solution 2

No. A near-zero random-effect variance or another boundary estimate can
make the random-effects structure unsupported. Inspect the site’s
convergence messages, variance components, observation counts and
design. Simplify or reconsider the model for a reason; do not remove
warnings merely to obtain a clean table.

## Recap

Choose the row meaning first. Keep actual time for irregular readings;
distinguish missing values, absent events and time outside observation.
Request the recipe’s shape when possible, and understand what
information a reshape cannot recover. A first-event hazard needs an
at-risk panel with follow-up stopped at the event. Repeated-measure
models instead retain repeated readings and model their dependence.
Check every site’s diagnostics and distinguish joint GLM estimation from
SLMA pooling.

**Next:** [17. Checking and recording a
study](https://isglobal-brge.github.io/dsOMOPClient/articles/book-17-reproduce.md).
