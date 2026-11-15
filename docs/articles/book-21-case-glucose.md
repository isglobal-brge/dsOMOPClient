# 21\. Case study: glucose trajectories

**What you will learn**

Choose a populated glucose measurement, describe trajectories and
observed target attainment, and fit a small Gaussian model.

**You need:** [the installed packages and practice
resources](https://isglobal-brge.github.io/dsOMOPClient/articles/book-00-start.html#before-you-start),
[concept
discovery](https://isglobal-brge.github.io/dsOMOPClient/articles/book-04-vocabulary.md),
and the preceding tutorial chapters. Run these sections in order in a
fresh `book-work` session. All observations are public synthetic
teaching data; the numerical results are not clinical evidence.

## Research question

Among Synthea people with blood-glucose measurements, how do first and
last values relate, and when is a declared teaching target first
observed? We select a lab-defined population, not a diagnosis-confirmed
diabetes cohort.

``` r
library(dsOMOPClient)
source("tools/book/display.R")
source("tools/book/connection.R")
source("tools/book/practice.R")
practice <- book_connect("numeric")
conns <- practice$conns
```

## Find the exact glucose concept

``` r
hits <- ds.omop.concept.search("Glucose", domain = "Measurement", vocabulary = "LOINC", conns = conns)$pooled
knitr::kable(hits[c("concept_name", "standard_concept", "concept_class_id")])
```

| concept\_name                                  | standard\_concept | concept\_class\_id |
| :--------------------------------------------- | :---------------- | :----------------- |
| Glucose \[Mass/volume\] in Blood               | S                 | Lab Test           |
| Glucose \[Mass/volume\] in Serum or Plasma     | S                 | Lab Test           |
| Glucose \[Presence\] in Urine by Test strip    | S                 | Lab Test           |
| Glucose \[Mass/volume\] in Urine by Test strip | S                 | Lab Test           |

``` r
glucose_id <- hits$concept_id[trimws(hits$concept_name) == "Glucose [Mass/volume] in Blood"]
knitr::kable(ds.omop.concept.locate(glucose_id, scope = "pooled", conns = conns)$pooled)
```

| concept\_id | table\_name | concept\_column          | n\_records | n\_persons |
| ----------: | :---------- | :----------------------- | ---------: | ---------: |
|     3000483 | measurement | measurement\_concept\_id |        225 |         10 |

``` r
knitr::kable(ds.omop.value.counts("measurement", "unit_concept_id", concept_id = glucose_id,
  scope = "pooled", conns = conns)$pooled)
```

|   n | concept\_name           | value |
| --: | :---------------------- | :---- |
| 225 | milligram per deciliter | 8840  |

``` r
mapping <- ds.omop.concept.relationships(glucose_id, relationship_id = "Maps to",
  conns = conns, symbol = "omop")$pooled
children <- ds.omop.concept.descendants(glucose_id, include_self = FALSE,
  conns = conns, symbol = "omop")$pooled
print(data.frame(Maps_to_rows = NROW(mapping), Descendant_rows = NROW(children)))
```

    #>   Maps_to_rows Descendant_rows
    #> 1            2               0

``` r
glucose_set <- omop_concept_set(glucose_id)
```

This populated Blood concept differs from Serum or Plasma. Its recorded
unit is mg/dL. The categories below are public teaching thresholds, not
treatment advice.

## Cohort, values and trajectories

``` r
glucose <- omop_event("measurement", glucose_set, unit = omop_unit(8840L))
first <- omop_variable(event = glucose, reducer = omop_reduce("first_value"))
last <- omop_variable(event = glucose, reducer = omop_reduce("last_value"))
cohort <- omop_population("measured", index = omop_index(glucose))
trajectory <- omop_recipe(populations = list(cohort), variables = list(
  first = first, last = last,
  control = omop_number_category(last, range = "thresholds", thresholds = c(70, 140)),
  change = omop_number_change(glucose),
  target_days = omop_number_change(glucose, "time_to_threshold", threshold = 140, op = "le")),
  outputs = omop_output("people", rows = "measured", format = "wide"))
book_symbols(recipe_execute(trajectory, out = c(people = "D_glucose"), conns = conns))
```

    #>      people 
    #> "D_glucose"

``` r
knitr::kable(book_summary(dsBaseClient::ds.summary("D_glucose$change", datasources = conns)))
```

| Site | Statistic |    Value |
| :--- | :-------- | -------: |
| site | 5%        | \-25.715 |
| site | 10%       | \-23.330 |
| site | 25%       | \-15.275 |
| site | 50%       |    9.050 |
| site | 75%       |   18.900 |
| site | 90%       |   23.130 |
| site | 95%       |   25.965 |
| site | Mean      |    2.580 |

All available history contributes to first and last. A difference in
person-level values is not a slope or an intervention effect. Selecting
people with a test can also select on disease severity and healthcare
use.

## Categories and time to target

The prescribed three-level rule produces a small category in this tiny
sample. This deliberately shows a refusal: the table is withheld, not a
zero-frequency result. Keep the planned thresholds and report that
limitation.

``` r
control_message <- tryCatch(ds.omop.table("D_glucose", "control", conns = conns),
  error = function(e) conditionMessage(e))
print(control_message)
```

    #> [1] "Every retained site must release a valid ds.table; pooled table refused."

An observed target time is missing for someone never observed below the
threshold. Day zero means already meeting it at the first reading.
Measurement schedules can move observed attainment later than the
underlying biological change.

``` r
knitr::kable(book_summary(dsBaseClient::ds.summary("D_glucose$target_days", datasources = conns)))
```

| Site | Statistic | Value |
| :--- | :-------- | ----: |
| site | 5%        |     0 |
| site | 10%       |     0 |
| site | 25%       |     0 |
| site | 50%       |     0 |
| site | 75%       |     0 |
| site | 90%       |     0 |
| site | 95%       |     0 |
| site | Mean      |     0 |

## Model the last value

Fit last glucose against first glucose. This small synthetic Gaussian
model teaches a workflow; it is not a validated diabetes risk model. The
local-start warnings are the documented dsBase setup behaviour, not
hidden output.

``` r
glucose_fit <- dsBaseClient::ds.glm("last ~ first", data = "D_glucose",
  family = "gaussian", datasources = conns)
```

    #> Warning: glm.fit: algorithm did not converge
    #> Warning: glm.fit: algorithm did not converge
    #> Warning: glm.fit: algorithm did not converge
    #> Warning: glm.fit: algorithm did not converge

``` r
knitr::kable(glucose_fit$coefficients)
```

|             |    Estimate | Std. Error |    z-value |   p-value |  low0.95CI |  high0.95CI |
| :---------- | ----------: | ---------: | ---------: | --------: | ---------: | ----------: |
| (Intercept) | 148.3870936 | 31.5706276 |   4.700163 | 0.0000026 |  86.509800 | 210.2643867 |
| first       | \-0.7482865 |  0.3761147 | \-1.989517 | 0.0466442 | \-1.485458 | \-0.0111152 |

## Exercises

**1.** Summarise first glucose in its declared unit.

Solution 1

``` r
knitr::kable(book_summary(dsBaseClient::ds.summary("D_glucose$first", datasources = conns)))
```

| Site | Statistic |  Value |
| :--- | :-------- | -----: |
| site | 5%        | 68.320 |
| site | 10%       | 69.940 |
| site | 25%       | 78.275 |
| site | 50%       | 84.550 |
| site | 75%       | 90.050 |
| site | 90%       | 92.980 |
| site | 95%       | 96.040 |
| site | Mean      | 83.400 |

The unit is mg/dL. A unit declaration must match the recorded source.

**2.** Explain why target-time missingness should not be replaced with
zero.

Solution 2

``` r
trajectory$variables$target_days$derived[c("method", "threshold", "op", "reference")]
```

    #> $method
    #> [1] "time_to_threshold"
    #> 
    #> $threshold
    #> [1] 140
    #> 
    #> $op
    #> [1] "le"
    #> 
    #> $reference
    #> [1] "first"

Zero represents immediate observed attainment. Missing instead means no
observed attainment in the eligible records; a censored analysis needs
follow-up times too.

**3.** Inspect the fitted sample size and convergence information.

Solution 3

``` r
glucose_fit[c("Nvalid", "Nmissing", "iter", "errorMessage")]
```

    #> $Nvalid
    #> [1] 10
    #> 
    #> $Nmissing
    #> [1] 0
    #> 
    #> $iter
    #> [1] 3
    #> 
    #> $errorMessage
    #>      ERROR MESSAGES
    #> site "No errors"

A successful recipe does not guarantee enough complete observations for
a model. Keep diagnostics beside the coefficients in a study record.

## Recap

Find a populated measurement, state the selected population, define
units and thresholds, preserve temporal meaning, then interpret a model
within those choices. Rare categories and small complete-case samples
can limit what is releasable.

**Next:** [22. Case study: drug
utilisation](https://isglobal-brge.github.io/dsOMOPClient/articles/book-22-case-drug.md).
