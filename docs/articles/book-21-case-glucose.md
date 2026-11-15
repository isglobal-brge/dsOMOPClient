# 21\. Case study: glucose trajectories

**What you will learn**

Describe first and later glucose readings, distinguish change from
slope, handle unobserved threshold attainment, and interpret a model
with only ten people.

**You need:** [the setup and numeric
resource](https://isglobal-brge.github.io/dsOMOPClient/articles/book-00-start.md),
and [the lab
lesson](https://isglobal-brge.github.io/dsOMOPClient/articles/book-14-labs.md).
Start a fresh R session in `book-work`.

## Research question

Among people with blood-glucose measurements, how do values change over
recorded follow-up? This is a lab-defined population, not a
diagnosis-confirmed diabetes cohort. The Synthea data are synthetic; the
cutoffs below are teaching rules.

``` r
library(dsOMOPClient)
source("tools/book/display.R")
source("tools/book/connection.R")
practice <- book_connect("numeric")
conns <- practice$conns
```

## Choose a populated measurement

``` r
hits <- ds.omop.concept.search("Glucose", domain = "Measurement", vocabulary = "LOINC", conns = conns)$pooled
knitr::kable(hits[c("concept_id", "concept_name", "standard_concept")])
```

| concept\_id | concept\_name                                  | standard\_concept |
| ----------: | :--------------------------------------------- | :---------------- |
|     3000483 | Glucose \[Mass/volume\] in Blood               | S                 |
|     3004501 | Glucose \[Mass/volume\] in Serum or Plasma     | S                 |
|     3009261 | Glucose \[Presence\] in Urine by Test strip    | S                 |
|     3024629 | Glucose \[Mass/volume\] in Urine by Test strip | S                 |

``` r
glucose_id <- hits$concept_id[trimws(hits$concept_name) == "Glucose [Mass/volume] in Blood"]
knitr::kable(ds.omop.concept.locate(glucose_id, scope = "pooled", conns = conns)$pooled)
```

| concept\_id | table\_name | concept\_column          | n\_records | n\_persons |
| ----------: | :---------- | :----------------------- | ---------: | ---------: |
|     3000483 | measurement | measurement\_concept\_id |        225 |         10 |

``` r
knitr::kable(ds.omop.value.counts("measurement", "unit_concept_id", concept_id = glucose_id,
  scope = "pooled", conns = conns)$pooled[c("value", "concept_name", "n")])
```

| value | concept\_name           |   n |
| :---- | :---------------------- | --: |
| 8840  | milligram per deciliter | 225 |

``` r
glucose_set <- omop_concept_set(glucose_id)
saveRDS(glucose_set, "glucose-concept.rds")
```

Blood glucose is **3000483**, recorded in **8840**, milligram per
deciliter (mg/dL). The locator shows a band of 225–229 rows and 10–14
people. The separate Serum or Plasma concept has records from only one
person in the public synthetic source; its occurrence cannot support a
released analysis. Do not combine specimens merely to increase the
sample.

## Select a baseline and a genuine later value

Index on first glucose. The last-value window starts the following day,
so a person with only a baseline reading cannot supply a follow-up
outcome. A slope uses all readings and their actual spacing; last minus
first ignores how long the change took.

``` r
glucose <- omop_event("measurement", glucose_set, unit = omop_unit(8840L))
first <- omop_variable(event = glucose, reducer = omop_reduce("first_value", window = omop_window(0, 0)))
last <- omop_variable(event = glucose, reducer = omop_reduce("last_value", window = omop_window(1, NULL)))
cohort <- omop_population("measured", index = omop_index(glucose))
trajectory <- omop_recipe(populations = list(cohort), variables = list(
  first = first, last = last,
  control = omop_number_bins(last, breaks = c(-Inf, 90, Inf), labels = c("below 90", "90 or above")),
  change = omop_number_change(glucose),
  per_year = omop_number(omop_number_change(glucose, "slope", time_unit = "days"),
    omop_number_spec(transform = omop_number_transform("affine", factor = 365.25))),
  followup_days = omop_variable(event = glucose, reducer = omop_reduce("last_date", window = omop_window(1, NULL))),
  target_days = omop_number_change(glucose, "time_to_threshold", threshold = 95, op = "ge")),
  outputs = omop_output("people", rows = "measured", format = "wide"))
invisible(recipe_execute(trajectory, out = c(people = "D_glucose"), conns = conns))
trajectory_summary <- do.call(rbind, lapply(c("first", "last", "change", "per_year", "followup_days"), function(column) {
  x <- book_summary(paste0("D_glucose$", column), datasources = conns)
  transform(x[x$Statistic %in% c("50%", "Mean"), ], Variable = column)
}))
knitr::kable(trajectory_summary, digits = 3, row.names = FALSE)
```

| Site | Statistic |    Value | N valid | N missing | Variable       |
| :--- | :-------- | -------: | ------: | --------: | :------------- |
| site | 50%       |   84.550 |      10 |         0 | first          |
| site | Mean      |   83.400 |      10 |         0 | first          |
| site | 50%       |   92.200 |      10 |         0 | last           |
| site | Mean      |   85.980 |      10 |         0 | last           |
| site | 50%       |    9.050 |      10 |         0 | change         |
| site | Mean      |    2.580 |      10 |         0 | change         |
| site | 50%       |    0.221 |      10 |         0 | per\_year      |
| site | Mean      |    0.397 |      10 |         0 | per\_year      |
| site | 50%       | 6562.500 |      10 |         0 | followup\_days |
| site | Mean      | 5915.200 |      10 |         0 | followup\_days |

All ten people have later values, with median follow-up about 18 years.
First and last means are 83.40 and 85.98 mg/dL; mean change is 2.58
mg/dL. Mean slope is 0.397 mg/dL per year (median 0.221). A change over
many years is not a short-term treatment response, and a straight-line
slope can miss fluctuations between readings.

## Classify values and distinguish unobserved attainment

``` r
knitr::kable(book_counts(dsBaseClient::ds.table("D_glucose$control", datasources = conns)))
```

| Category    | Count |
| :---------- | ----: |
| below 90    |     4 |
| 90 or above |     6 |
| NA          |     0 |

Four people are below 90 and six are at least 90 mg/dL. It is a
presentation rule, not a diagnosis or treatment goal. More detailed
categories may be withheld in a population this small.

For a separate timing question, use the first **observed** glucose ≥95
mg/dL. In this tiny synthetic dataset, a 100 mg/dL rule would leave too
few people for a released summary, so the lesson uses 95 mg/dL as a
teaching threshold. In a real study the threshold comes from the
protocol, and a withheld result is reported as withheld, not avoided by
changing the cutoff.

``` r
knitr::kable(book_summary("D_glucose$target_days", datasources = conns), digits = 3)
```

| Site | Statistic |  Value | N valid | N missing |
| :--- | :-------- | -----: | ------: | --------: |
| site | 5%        |  259.7 |       8 |         2 |
| site | 10%       |  519.4 |       8 |         2 |
| site | 25%       |  986.5 |       8 |         2 |
| site | 50%       | 1169.0 |       8 |         2 |
| site | 75%       | 2693.5 |       8 |         2 |
| site | 90%       | 4769.8 |       8 |         2 |
| site | 95%       | 5555.9 |       8 |         2 |
| site | Mean      | 2101.5 |       8 |         2 |

Eight people have an observed attainment and two are missing. The median
among attainments is 1,169 days; the mean is 2,101.5 days. One of the
eight attains at day zero, which is not shown directly by these
interpolated quantiles. A zero means the threshold was met at baseline;
a positive time means a later recorded crossing. Missing means no
crossing was observed during available measurements. The missing people
must remain under observation until censoring in a time-to-event
analysis; a mean among observed crossings alone is conditional on
crossing and does not estimate everyone’s time to threshold.

## Model last against first with appropriate uncertainty

A Gaussian regression describes the association between first and last
readings. With ten complete people and two coefficients, use a t
interval with eight residual degrees of freedom rather than the
large-sample z interval.

``` r
fit <- dsBaseClient::ds.glm("last ~ first", data = "D_glucose", family = "gaussian", datasources = conns)
b <- fit$coefficients["first", "Estimate"]
se <- fit$coefficients["first", "Std. Error"]
knitr::kable(data.frame(N = fit$Nvalid, df = fit$df, Estimate = b, Lower_95 = b - qt(.975, fit$df) * se,
  Upper_95 = b + qt(.975, fit$df) * se, P_t = 2 * pt(-abs(b / se), df = fit$df)), digits = 3)
```

|  N | df | Estimate | Lower\_95 | Upper\_95 |  P\_t |
| -: | -: | -------: | --------: | --------: | ----: |
| 10 |  8 |  \-0.748 |   \-1.616 |     0.119 | 0.082 |

The coefficient is about −0.75: a 1 mg/dL higher first value is
associated with about 0.75 mg/dL lower last value. The t-based interval
includes zero: the 95% interval is −1.616 to 0.119 and the t-based
p-value is 0.082. Regression to the mean, unequal follow-up and the
sample of only ten people make this association fragile; it is not
evidence that high baseline glucose causes improvement.

## Exercises

**1. Change the time unit.** Express the observed threshold times in
years and retain the number with missing attainment.

Solution 1

``` r
invisible(ds.omop.mutate("D_glucose", target_years = target_days / 365.25,
  newobj = "D_glucose_years", conns = conns))
knitr::kable(book_summary("D_glucose_years$target_years", datasources = conns), digits = 3)
```

| Site | Statistic |  Value | N valid | N missing |
| :--- | :-------- | -----: | ------: | --------: |
| site | 5%        |  0.711 |       8 |         2 |
| site | 10%       |  1.422 |       8 |         2 |
| site | 25%       |  2.701 |       8 |         2 |
| site | 50%       |  3.201 |       8 |         2 |
| site | 75%       |  7.374 |       8 |         2 |
| site | 90%       | 13.059 |       8 |         2 |
| site | 95%       | 15.211 |       8 |         2 |
| site | Mean      |  5.754 |       8 |         2 |

The median becomes 3.20 years and the mean 5.75 years. Divide the day
summary by 365.25 to check the values. The valid and missing counts do
not change: rescaling cannot supply unobserved crossings.

**2. Separate endpoints from a trajectory.** Calculate last minus first
on the server and compare with the native change summary. Does agreement
prove a steady increase?

Solution 2

``` r
invisible(ds.omop.mutate("D_glucose", difference = last - first,
  newobj = "D_difference", conns = conns))
knitr::kable(book_summary("D_difference$difference", datasources = conns), digits = 3)
```

| Site | Statistic |    Value | N valid | N missing |
| :--- | :-------- | -------: | ------: | --------: |
| site | 5%        | \-25.715 |      10 |         0 |
| site | 10%       | \-23.330 |      10 |         0 |
| site | 25%       | \-15.275 |      10 |         0 |
| site | 50%       |    9.050 |      10 |         0 |
| site | 75%       |   18.900 |      10 |         0 |
| site | 90%       |   23.130 |      10 |         0 |
| site | 95%       |   25.965 |      10 |         0 |
| site | Mean      |    2.580 |      10 |         0 |

The difference agrees with native change because both calculate last
minus first. All ten people have later readings, so the complete-case
populations also coincide. It describes only two endpoints; intermediate
values can rise and fall. The slope uses those intermediate observations
but still assumes a straight line.

## Recap

Retain real follow-up, state the time scale, and distinguish observed
crossings from censoring. Small datasets can support a useful
descriptive analysis without supporting a precise or clinically
predictive model.

**Next:** [22. Case study: drug
utilisation](https://isglobal-brge.github.io/dsOMOPClient/articles/book-22-case-drug.md).
