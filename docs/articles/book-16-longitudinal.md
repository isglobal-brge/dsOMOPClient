# 16\. Longitudinal data: from events to analysis

**What you will learn**

Choose wide, long or person-period data; handle irregular measurements
and missing periods; reshape privately; and fit a discrete-time hazard
model and repeated-measure mixed models across two connections.

**You need:** the Opal login from [Getting
started](https://isglobal-brge.github.io/dsOMOPClient/articles/book-00-start.md),
[output
formats](https://isglobal-brge.github.io/dsOMOPClient/articles/book-08-outputs.md)
and [lab
values](https://isglobal-brge.github.io/dsOMOPClient/articles/book-14-labs.md).
Run every section in order, including solutions. This chapter uses
`omop.gibleed`, `omop.synthea` and the two Synthea sites. GiBleed and
Synthea are public **synthetic** resources. Their results teach analysis
mechanics, not clinical conclusions.

## The same people, two shapes

*Longitudinal* means following the same people over time. A small
invented example: three people measured on two days. Days are relative
to each person’s entry.

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

**Check.** Which shape would you use to compare baseline means? Which
for a model of change over time?

Worked answer

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
conns <- ds.omop.login("https://localhost:8443", user = "administrator",
  password = "password", resource = "omop.gibleed", server = "site")$conns
```

``` r
visits <- omop_event("visit_occurrence", concepts = 9201L, name = "admission")
visitors <- omop_population("visitors", index = omop_index(visits))
visit_recipe <- omop_recipe(events = list(visits), populations = list(visitors),
  outputs = omop_output("visits", rows = "visitors", format = "long", sources = "admission",
    options = list(columns_derived = c("days_from_index", "duration_days"))))
recipe_execute(visit_recipe, out = c(visits = "D_visit_long"), datasources = conns)
```

The long table remains private. Release the setting counts to describe
its rows.

``` r
visit_counts <- dsBaseClient::ds.table("D_visit_long$concept_id", datasources = conns)
visit_counts$output.list$TABLES.COMBINED_all.sources_counts
```

    #> D_visit_long$concept_id
    #> Inpatient_Visit              NA
    #>            1037               0

The table counts exactly 1,037 admission rows; chapter 4’s locator count
was floored to the five-row band starting at 1,035. To count people
separately, reduce each visitor’s records to a count and distinguish
single from repeat visitors.

``` r
visits_per_person <- omop_study(visits, outputs = "baseline", baseline = list(
  admissions = omop_variable(event = visits, reducer = omop_reduce("count"))))
recipe_execute(visits_per_person, out = c(baseline = "D_visitors"), datasources = conns)
ds.omop.mutate("D_visitors", pattern = ifelse(admissions == 1,
  "One admission", "Repeated admissions"), newobj = "D_visit_patterns", datasources = conns)
dsBaseClient::ds.table("D_visit_patterns$pattern",
  datasources = conns)$output.list$TABLES.COMBINED_all.sources_counts
```

    #> D_visit_patterns$pattern
    #>       One admission Repeated admissions                  NA
    #>                 756                 134                   0

There are 890 visitors: 756 with one admission and 134 with repeated
admissions. A long table does not guarantee repeated visits for every
person. Inpatient Visit is concept **9201**, found in chapter 4.

Summarise time since entry without fetching event rows.

Worked answer

``` r
visit_time <- dsBaseClient::ds.mean("D_visit_long$days_from_index", type = "combine", datasources = conns)
visit_time$Global.Mean
```

|                 | EstimatedMean | Nmissing | Nvalid | Ntotal |
| :-------------- | ------------: | -------: | -----: | -----: |
| studiesCombined |       1272.55 |        0 |   1037 |   1037 |

The mean is about 1273 days: the first admission contributes zero, and
subsequent admissions contribute positive offsets. Some people still
have only one admission. This is an event-weighted mean, not the mean
gap between admissions. Switch to Synthea for repeated numeric
measurements.

## Irregular measurements: define windows first

``` r
ds.omop.disconnect(datasources = conns)
ds.omop.connect(resource = "omop.synthea", datasources = conns)
```

``` r
lab <- omop_event("measurement", concepts = 3000963L, value = "value_as_number",
  unit = omop_unit(8713L), name = "hb")
entry <- omop_population("entry", index = omop_index(lab))
```

Haemoglobin **3000963** and g/dL **8713** are the measurement and unit
used in chapter 14. Index is each person’s first haemoglobin. These
synthetic records are irregularly spaced over years. Use three
**1000-day windows**, not calendar years. Window 1 includes days 0–999,
window 2 days 1000–1999, and window 3 days 2000–2999. First and last
refer to actual dates within each window, not scheduled visits.

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
recipe_execute(windows, out = c(wide = "D_windows"), datasources = conns)
```

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
do.call(rbind, window_results)
```

| Period |  First |   Last | Observed | Missing |
| -----: | -----: | -----: | -------: | ------: |
|      1 | 13.986 | 14.286 |       28 |       0 |
|      2 | 15.300 | 15.300 |       10 |      18 |
|      3 | 14.567 | 14.567 |       15 |      13 |

In periods 2 and 3, first equals last (15.30 and 14.57 g/dL): each
observed person has only one reading in those windows. In period 1, some
have repeated readings. These means use the observed people in each
period; changing means can therefore reflect changing membership. A
missing measurement does not prove that the person left observation.
Check observation coverage separately for a follow-up analysis.

**Check.** Make missingness a named category for each period and release
its counts.

Worked answer

``` r
ds.omop.mutate("D_windows",
  missing1 = ifelse(is.na(first1), "missing", "observed"),
  missing2 = ifelse(is.na(first2), "missing", "observed"),
  missing3 = ifelse(is.na(first3), "missing", "observed"),
  newobj = "D_missing", datasources = conns)
dsBaseClient::ds.table("D_missing$missing1",
  datasources = conns)$output.list$TABLES.COMBINED_all.sources_counts
```

    #> D_missing$missing1
    #> observed       NA
    #>       28        0

``` r
dsBaseClient::ds.table("D_missing$missing2",
  datasources = conns)$output.list$TABLES.COMBINED_all.sources_counts
```

    #> D_missing$missing2
    #>  missing observed       NA
    #>       18       10        0

``` r
dsBaseClient::ds.table("D_missing$missing3",
  datasources = conns)$output.list$TABLES.COMBINED_all.sources_counts
```

    #> D_missing$missing3
    #>  missing observed       NA
    #>       13       15        0

The tables show 28, 10 and 15 observed people: therefore 0, 18 and 13
missing values. Do not carry a measurement forward without declaring how
long it remains valid and what that assumption means.

## Wide columns at prespecified time points

A wide table can hold several scheduled values for each person. A named
grid makes the target times, tolerances and selection rule explicit.
Each value comes from an eligible observed event; no measurement is
interpolated.

``` r
points <- omop_timepoints(list(
  baseline = omop_timepoint(),
  year5 = omop_timepoint(5, "years", tolerance = c(-365, 365))),
  anchor = "index", reuse = FALSE)
repeated_hb <- omop_repeated(lab, values = list(hb = omop_number_rows(lab)),
  timepoints = points, missing = "keep")
repeated_recipe <- omop_recipe(events = list(lab), populations = list(entry),
  variables = list(measurements = repeated_hb),
  outputs = omop_output("wide", rows = "entry", format = "wide"))
repeated_result <- recipe_execute(repeated_recipe, out = "D_timepoints", datasources = conns)
dsBaseClient::ds.colnames("D_timepoints", datasources = conns)
```

    #> $site
    #> [1] "row_id"                   "cohort_row_id"            "person_id"
    #> [4] "measurements_hb_baseline" "measurements_hb_year5"

The generated names retain the measurement and time-point names.
`closest` is the default selection; `first` or `last` would choose
differently within the inclusive tolerance. `reuse = FALSE` prevents an
event from filling two points. The tolerance here is a teaching choice
for irregular data, not a claim that a lab a year away represents a
clinical five-year visit.

**Check.** Inspect the five-year column’s released summary and missing
count.

Solution: a scheduled column

``` r
dsBaseClient::ds.summary(
  "D_timepoints$measurements_hb_year5", datasources = conns)
```

    #> $site
    #> $site$class
    #> [1] "numeric"
    #>
    #> $site$length
    #> [1] 28
    #>
    #> $site$`quantiles & mean`
    #>       5%      10%      25%      50%      75%      90%      95%     Mean
    #> 12.59000 12.88000 13.25000 16.00000 16.75000 17.20000 17.29000 15.15333

``` r
dsBaseClient::ds.numNA("D_timepoints$measurements_hb_year5", datasources = conns)
```

    #> $site
    #> [1] 13

The five-year point has 15 observed and 13 missing values, with a mean
of 15.15 g/dL among those observed. The tolerance is ±365 days; this
selects a nearby observation without inventing a scheduled reading.

**Check.** Where can you check the expanded columns’ types before
modelling?

Solution: the published table codebook

``` r
recipe_codebook(repeated_result)[[1]]$columns[c("name", "r_type")]
```

| name                       | r\_type   |
| :------------------------- | :-------- |
| row\_id                    | character |
| cohort\_row\_id            | character |
| person\_id                 | character |
| measurements\_hb\_baseline | double    |
| measurements\_hb\_year5    | double    |

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
server’s allowed privacy profile; our server allows it.

For a dense panel with last values and event counts, ask the recipe
directly. The native panel supports `last` and `mean`, but not a `first`
analysis. The wide window reducers above provide first values explicitly
without inventing a panel option.

``` r
panel <- omop_recipe(events = list(lab), populations = list(entry),
  outputs = omop_output("panel", rows = "entry", format = "person_period",
    options = list(sources = list(lab), bins = list(width = 1000L),
      window = omop_window(0, 2999), analyses = c("count", "last"), representation = "dense")))
recipe_execute(panel, out = c(panel = "D_panel"), datasources = conns)
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

**Exercise 1.** Verify that the reshape kept three rows per person,
without reading rows.

Worked answer

``` r
period_counts <- dsBaseClient::ds.table("D_reshaped$period", datasources = conns)
period_counts$output.list$TABLES.COMBINED_all.sources_counts
```

    #> D_reshaped$period
    #>  1  2  3 NA
    #> 28 28 28  0

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

For this section use the same two Synthea sites as chapter 14: 14
different people at each. No people are duplicated; a real multisite
study must establish nonoverlapping populations separately. As there,
the second connection uses the handle `pair`, and the earlier `conns`
connection stays available for the later single-site exercises.

``` r
two_conns <- ds.omop.login(rep("https://localhost:8443", 2), user = "administrator",
  password = "password",
  server = c("north", "south"),
  resource = c(north = "omop.synthea_north", south = "omop.synthea_south"),
  symbol = "pair")$conns
```

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
        how = "left", cardinality = "many_to_one"))), publish = "risk")
recipe_execute(hazard, out = c(risk = "D_days"), datasources = two_conns, symbol = "pair")
```

Remove days after the first repeat or death. A missing next-day value
means “no subsequent lab found”, so retain observed days until another
censor applies. We count an event on the death day if both dates
coincide; state this tie rule.

``` r
ds.omop.mutate("D_days", to_next = next_day - period_start_days,
  to_death = death_day - period_start_days,
  newobj = "D_remaining", datasources = two_conns, symbol = "pair")
ds.omop.filter("D_remaining",
  expr = omop_where((is.na(to_next) | to_next >= 0) &
  (is.na(to_death) | to_death >= 0)),
  newobj = "D_risk", datasources = two_conns, symbol = "pair")
ds.omop.mutate("D_risk", event = ifelse(hb_count > 0, 1L, 0L),
  years = period_start_days / 365.25,
  newobj = "D_hazard", datasources = two_conns, symbol = "pair")
hazard_counts <- dsBaseClient::ds.table("D_hazard$event", datasources = two_conns)
hazard_counts$output.list$TABLES.COMBINED_all.sources_counts
```

    #> D_hazard$event
    #>     0     1    NA
    #> 48535    25     0

Each row is one observed day at risk. More than one lab on that day is
still one outcome. Time in years is a predictor; the outcome probability
remains **per day**. The binomial GLM models log odds with a linear time
trend. A study would assess that time specification and the assumption
of noninformative censoring.

``` r
hazard_fit <- dsBaseClient::ds.glm("event ~ years", data = "D_hazard",
  family = "binomial", datasources = two_conns)
hazard_table <- as.data.frame(hazard_fit$coefficients[, 1:4, drop = FALSE])
hazard_table[["p-value"]] <- format.pval(hazard_table[["p-value"]], digits = 3, eps = 1e-16)
hazard_table
```

|             |  Estimate | Std. Error |   z-value | p-value  |
| :---------- | --------: | ---------: | --------: | :------- |
| (Intercept) | \-12.9553 |     1.2056 | \-10.7463 | \< 1e-16 |
| years       |    1.2945 |     0.2344 |    5.5221 | 3.35e-08 |

The event table has 25 first repeats among 28 people. The positive time
coefficient means a repeat becomes more likely on a given day among
those still without a repeat, under the assumed linear log-odds trend.

This is a jointly estimated distributed GLM. Its risk-set likelihood has
one first event at most per person; it is not a logistic model of every
recurrent reading treated as an independent patient.

**Exercise 2.** Show the fitted odds ratio per additional year since
entry. Does it mean a yearly probability of getting a lab?

Worked answer

``` r
b <- hazard_fit$coefficients["years", 1]
se <- hazard_fit$coefficients["years", 2]
data.frame(Daily_odds_ratio = exp(b), Lower_95 = exp(b - 1.96 * se),
  Upper_95 = exp(b + 1.96 * se))
```

| Daily\_odds\_ratio | Lower\_95 | Upper\_95 |
| -----------------: | --------: | --------: |
|              3.649 |     2.305 |     5.777 |

The daily odds ratio is 3.65 (95% CI 2.31–5.78) per additional year
since entry. It compares the odds of a repeat **on the current day**,
conditional on having remained at risk, for a one-year difference in
time since entry. It is not a causal effect or an annual risk. The event
is recorded care, not disease onset.

## Repeated measures and mixed models

Use the measured BMI concept and kg/m² unit found, inspected and saved
in [chapter 4’s measured BMI
discovery](https://isglobal-brge.github.io/dsOMOPClient/articles/book-04-vocabulary.html#find-measured-bmi-and-its-recorded-unit).
The measurement is Body mass index **3038553**, with unit **9531**,
kg/m². For actual repeated-value modelling use Synthea’s BMI readings,
whose within-person trajectories provide a more useful random-intercept
example than hemoglobin here. A *random intercept* lets people have
different baseline levels and accounts for correlation between their
repeated readings. This is a different question from time to the first
repeat lab.

``` r
bmi <- omop_event("measurement", concepts = 3038553L, value = "value_as_number",
  unit = omop_unit(9531L), name = "bmi")
bmi_entry <- omop_population("bmi_entry", index = omop_index(bmi))
repeated <- omop_recipe(events = list(bmi), populations = list(bmi_entry),
  outputs = omop_output("long", rows = "bmi_entry", format = "long", sources = "bmi",
    options = list(columns_derived = c("value", "unit", "days_since_previous"))))
recipe_execute(repeated, out = c(long = "D_long"), datasources = two_conns, symbol = "pair")
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
these models. With multiple episodes per person, use `person_id` to
group repeated observations of the same person; `cohort_row_id`
identifies an episode.

``` r
ds.omop.mutate("D_long", years = start_date / 365.25,
  above_cutoff = ifelse(value >= 25, 1L, 0L),
  newobj = "D_repeat", datasources = two_conns, symbol = "pair")
repeat_groups <- dsBaseClient::ds.meanSdGp("D_repeat$value", "D_repeat$above_cutoff",
  type = "combine", datasources = two_conns)
data.frame(Band = c("below 25", "25 or above"),
  Readings = as.numeric(repeat_groups$Nvalid_gp),
  Mean_BMI = as.numeric(repeat_groups$Mean_gp))
```

| Band        | Readings | Mean\_BMI |
| :---------- | -------: | --------: |
| below 25    |      123 |    19.130 |
| 25 or above |      315 |    28.362 |

The 438 readings split into 123 below and 315 at or above 25. Each
mean’s denominator is the number of readings in that band, not the
number of people. People with more readings contribute more to these
means. The cutoff is a teaching rule only: this synthetic population
includes children, and an adult BMI cutoff is not an age-specific
clinical classification. The continuous and binary responses answer
different questions.

`SLMA` means **study-level meta-analysis**. Each server fits its own
mixed model; only estimates and standard errors return for pooling. It
is not a single mixed model fitted jointly to concatenated rows. A
fixed-effect pool assumes a common coefficient. A random-effects pool
allows site coefficients to differ; here use REML to estimate their
between-site variance. With only two sites, that variance and the pooled
interval remain uncertain.

Optimizer convergence alone does not establish that the data support a
model; the binary example below illustrates this.

``` r
linear_mixed <- dsBaseClient::ds.lmerSLMA("value ~ years + (1|cohort_row_id)",
  dataName = "D_repeat", datasources = two_conns)
site_slopes <- data.frame(Site = names(two_conns),
  Estimate = as.numeric(linear_mixed$betamatrix.all["years", ]),
  SE = as.numeric(linear_mixed$sematrix.all["years", ]))
site_slopes
```

| Site  | Estimate |     SE |
| :---- | -------: | -----: |
| north |   0.2008 | 0.0211 |
| south |   0.0938 | 0.0224 |

``` r
pooled <- linear_mixed$SLMA.pooled.ests.matrix
pooled[, c("pooled.FE", "se.FE", "pooled.REML", "se.REML"), drop = FALSE]
```

|             | pooled.FE |  se.FE | pooled.REML | se.REML |
| :---------- | --------: | -----: | ----------: | ------: |
| (Intercept) |   23.3505 | 0.9636 |     23.3462 |  1.3860 |
| years       |    0.1507 | 0.0154 |      0.1476 |  0.0535 |

The site slopes are about 0.201 and 0.094 BMI units per year. They
differ strongly (I² about 92%); a common-effect standard error
understates this disagreement. The REML standard error is about 3.5
times the fixed-effect one. Interpret the random-effects estimate and
interval, while remembering that two sites provide little information
about the distribution of site effects. Growth may also be nonlinear, so
this straight-line model is only a demonstration.

``` r
b <- pooled["years", "pooled.REML"]
se <- pooled["years", "se.REML"]
data.frame(BMI_units_per_year = b, Lower_95 = b - 1.96 * se,
  Upper_95 = b + 1.96 * se)
```

| BMI\_units\_per\_year | Lower\_95 | Upper\_95 |
| --------------------: | --------: | --------: |
|                 0.148 |     0.043 |     0.252 |

The average slope is 0.148 BMI units per year (95% normal interval
0.043–0.252). The interval expresses uncertainty about the average
site slope under the random-effects model. It is not the range of
individual people’s slopes.

## A binary model the data do not support well

Turning BMI into a ≥25 flag changes the question and discards
information. In the public synthetic records, 19 of 27 measured people
stay on one side of this cutoff throughout follow-up. The cutoff is a
teaching choice, not a clinical classification for this population,
which includes children.

``` r
binary_mixed <- dsBaseClient::ds.glmerSLMA("above_cutoff ~ years + (1|cohort_row_id)",
  family = "binomial", dataName = "D_repeat", datasources = two_conns)
binary_sites <- data.frame(Site = names(two_conns),
  Intercept = as.numeric(binary_mixed$betamatrix.all["(Intercept)", ]),
  Time_coefficient = as.numeric(binary_mixed$betamatrix.all["years", ]),
  Random_intercept_SD = vapply(binary_mixed$output.summary[c("study1", "study2")],
    function(x) as.data.frame(x$varcor)$sdcor[1], numeric(1)))
binary_sites
```

| Site  | Intercept | Time\_coefficient | Random\_intercept\_SD |
| :---- | --------: | ----------------: | --------------------: |
| north |    \-9.38 |              1.28 |                 17.32 |
| south |      9.25 |              0.00 |                 12.43 |

Site intercepts near −9.4 and +9.2 and random-intercept standard
deviations near 17 and 12 logits indicate extreme separation between
people. Even an optimizer that reports convergence does not make these
data informative for this model. We do not interpret a pooled binary
effect here.

**Exercise 3.** Would changing the pooling method repair this binary
model?

Solution: assess support before pooling

No. The extreme site estimates arise before pooling. Inspect
within-person outcome variation and site variance components; use the
continuous response if it answers the question, or reconsider the study
design. A different pooling method cannot create missing information.

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
