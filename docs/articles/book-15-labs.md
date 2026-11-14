# 15\. Working with lab values

**What you will learn**

Find a lab and its units; choose one value per person; convert units;
form bins and flags; measure change; and pool summaries and a model
across two connections.

**You need:** the packages, downloaded helpers and two loaded resources
in [Before you
start](https://isglobal-brge.github.io/dsOMOPClient/articles/book-00-start.html#before-you-start),
plus the recipe basics from
[chapter 6](https://isglobal-brge.github.io/dsOMOPClient/articles/book-06-events.md).
Start a fresh R session in `book-work`. Run the sections in order: each
uses objects defined immediately above it.

These are **public synthetic Synthea27Nj patients**, from the pinned
Eunomia archive used by the numeric validation loader. There are 28
fictional people, not 27. GiBleed is the other practice resource and has
no populated numeric measurement values. All results below were executed
with dsOMOP 2.10.4.

## Connect and find a lab

The shared connection file selects the driver, URL, project and resource
names. Selecting `numeric` switches this lesson to Synthea without
changing those settings.

``` r
library(dsOMOPClient)
source("tools/book/display.R")
source("tools/book/connection.R")
source("tools/book/practice.R")
practice <- book_connect("numeric")
conns <- practice$conns
names(conns)
```

    #> [1] "site"

Search the vocabulary first. A *concept* identifies what was measured;
it does not identify a patient. Hemoglobin is the vocabulary’s spelling
of haemoglobin.

``` r
hits <- ds.omop.concept.search("Hemoglobin", conns = conns)
knitr::kable(head(hits$pooled[c("concept_name", "domain_id", "vocabulary_id")], 6))
```

| concept\_name                             | domain\_id | vocabulary\_id |
| :---------------------------------------- | :--------- | :------------- |
| international unit per gram of hemoglobin | Unit       | UCUM           |
| microgram per gram of hemoglobin          | Unit       | UCUM           |
| unit per gram of hemoglobin               | Unit       | UCUM           |
| micromole per gram of hemoglobin          | Unit       | UCUM           |
| micromole per hour and gram of hemoglobin | Unit       | UCUM           |
| micromole per mole of hemoglobin          | Unit       | UCUM           |

Select the blood mass-concentration concept deliberately. Other
hemoglobin concepts can describe a different test. IDs in code select
public vocabulary; shown tables use their names.

``` r
lab_id <- 3000963L
chosen <- ds.omop.concept.lookup(lab_id, conns = conns)
knitr::kable(chosen$pooled[c("concept_name", "domain_id")])
```

| concept\_name                       | domain\_id  |
| :---------------------------------- | :---------- |
| Hemoglobin \[Mass/volume\] in Blood | Measurement |

Now ask which units were recorded for this concept. This is a checked
aggregate, not a download of measurement rows.

``` r
unit_counts <- ds.omop.value.counts("measurement", "unit_concept_id",
  concept_id = lab_id, scope = "pooled", conns = conns)
knitr::kable(unit_counts$pooled[c("concept_name", "n")])
```

| concept\_name      |   n |
| :----------------- | --: |
| gram per deciliter | 105 |

The record count is disclosure-banded; it is not an exact patient count.
The pinned public source contains 108 hemoglobin readings in grams per
deciliter (g/dL), independently verified by the administrator loader.
The analyst sees only the protected aggregate above.

**Exercise.** Look up the name of unit 8713. Does a vocabulary lookup
prove that any patient has a result in that unit?

Solution

``` r
unit_names <- ds.omop.concept.lookup(8713L, conns = conns)
knitr::kable(unit_names$pooled["concept_name"])
```

| concept\_name      |
| :----------------- |
| gram per deciliter |

No. The lookup describes the unit; the preceding aggregate establishes
recorded use.

## Choose one value per person

An *event* selects records. Here it selects one lab and one recorded
unit. A unit filter excludes other units; it does not convert them.

``` r
lab <- omop_event("measurement", concepts = lab_id, value = "value_as_number",
  where = omop_where(unit_concept_id == 8713L))
class(lab)
```

    #> [1] "omop_event" "list"

A *reducer* turns repeated readings into one number. First and last use
the record dates (then source and record key for ties), whereas min and
max use the size of the value. These reducers skip missing values by
default.

``` r
first <- omop_variable(event = lab, reducer = omop_reduce("first_value"))
last <- omop_variable(event = lab, reducer = omop_reduce("last_value"))
class(first)
```

    #> [1] "omop_variable" "list"

The mean weights each available reading equally **within** a person. Min
and max show that person’s observed range, not a clinical reference
range.

``` r
average <- omop_variable(event = lab, reducer = omop_reduce("mean"))
minimum <- omop_variable(event = lab, reducer = omop_reduce("min"))
maximum <- omop_variable(event = lab, reducer = omop_reduce("max"))
class(average)
```

    #> [1] "omop_variable" "list"

Omitting the window on these advanced reducers uses all recorded
history. Assign a wide table, with one private row per person. Only its
symbol returns.

``` r
values <- omop_recipe(variables = list(first = first, last = last,
  mean = average, minimum = minimum, maximum = maximum),
  outputs = omop_output("people", format = "wide", grain = "person"))
stopifnot(recipe_explain(values, conns = conns)$selected == "v2")
print(recipe_execute(values, out = c(people = "D_labs"), conns = conns))
```

    #>   people 
    #> "D_labs"

Ask for an approved summary of the first values. All patient-level
numbers remain on the server; the table below contains summary
statistics only.

``` r
first_summary <- dsBaseClient::ds.summary("D_labs$first", datasources = conns)
knitr::kable(book_summary(first_summary))
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

Full aggregate response

``` r
first_summary
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
    #> 11.29000 12.48000 12.97500 13.70000 15.42500 16.19000 16.85500 13.98571

Compare the last-value distribution with the first-value distribution. A
change in these summaries alone does not describe each individual’s
change.

``` r
last_summary <- dsBaseClient::ds.summary("D_labs$last", datasources = conns)
knitr::kable(book_summary(last_summary))
```

| Site | Statistic |    Value |
| :--- | :-------- | -------: |
| site | 5%        | 12.34000 |
| site | 10%       | 12.74000 |
| site | 25%       | 13.60000 |
| site | 50%       | 15.00000 |
| site | 75%       | 16.25000 |
| site | 90%       | 16.83000 |
| site | 95%       | 17.03000 |
| site | Mean      | 14.89286 |

The next mean gives each person equal weight, regardless of their number
of readings. It is not the mean over all measurement rows.

``` r
mean_summary <- dsBaseClient::ds.summary("D_labs$mean", datasources = conns)
knitr::kable(book_summary(mean_summary))
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

**Exercise 1.** Summarise the minimum and maximum columns. Why can first
differ from minimum?

Solution 1

``` r
minimum_summary <- dsBaseClient::ds.summary("D_labs$minimum", datasources = conns)
maximum_summary <- dsBaseClient::ds.summary("D_labs$maximum", datasources = conns)
knitr::kable(rbind(transform(book_summary(minimum_summary), Column = "minimum"),
                  transform(book_summary(maximum_summary), Column = "maximum")))
```

| Site | Statistic |    Value | Column  |
| :--- | :-------- | -------: | :------ |
| site | 5%        | 10.83500 | minimum |
| site | 10%       | 11.25000 | minimum |
| site | 25%       | 12.17500 | minimum |
| site | 50%       | 12.90000 | minimum |
| site | 75%       | 13.45000 | minimum |
| site | 90%       | 14.46000 | minimum |
| site | 95%       | 14.66500 | minimum |
| site | Mean      | 12.74643 | minimum |
| site | 5%        | 13.50000 | maximum |
| site | 10%       | 15.01000 | maximum |
| site | 25%       | 15.62500 | maximum |
| site | 50%       | 16.45000 | maximum |
| site | 75%       | 16.92500 | maximum |
| site | 90%       | 17.20000 | maximum |
| site | 95%       | 17.26500 | maximum |
| site | Mean      | 16.10357 | maximum |

First is earliest in time; minimum is smallest in value. They answer
different questions.

**Exercise 2.** Would a person with no eligible lab have first = 0?

Solution 2

No. First, last, mean, min and max are missing (`NA`) when no eligible
nonmissing values exist. Zero would claim a measured value. The
person-grain table still includes that person; this particular dataset
has hemoglobin for everyone.

## Anchor a value and estimate a slope

An *index date* is the reference date for each study episode. For this
teaching example, choose seven days after each person’s first lab. The
offset makes it clear that “at index” can use a nearby measurement
rather than an exact-date match.

``` r
index <- omop_index(lab, offset = 7L)
class(index)
```

    #> [1] "omop_index" "list"

Tolerance `c(-7, 0)` includes the preceding seven days and index day,
never a later reading. The shortcut selects the closest eligible
nonmissing value.

``` r
indexed <- omop_study(index, outputs = "baseline", baseline = list(
  at_index = omop_value_at_index("measurement", lab_id,
    tolerance = c(-7, 0), unit = 8713L)))
stopifnot(recipe_explain(indexed, conns = conns)$selected == "v2")
print(recipe_execute(indexed, out = c(baseline = "D_lab_index"), conns = conns))
```

    #>      baseline 
    #> "D_lab_index"

Read its summary. If no measurement falls inside the tolerance the value
is missing, even when that person has measurements elsewhere in their
history.

``` r
index_summary <- dsBaseClient::ds.summary("D_lab_index$at_index", datasources = conns)
knitr::kable(book_summary(index_summary))
```

| Site | Statistic |  Value |
| :--- | :-------- | -----: |
| site | 5%        | 11.810 |
| site | 10%       | 12.480 |
| site | 25%       | 12.975 |
| site | 50%       | 13.700 |
| site | 75%       | 15.425 |
| site | 90%       | 16.190 |
| site | 95%       | 16.855 |
| site | Mean      | 14.150 |

A *slope* fits a straight line to value against date within each person.
It uses all eligible readings, unlike first-to-last subtraction. The
unit here is g/dL **per day**. At least two distinct measurement dates
are needed.

``` r
slopes <- omop_recipe(variables = list(per_day = omop_variable(event = lab,
  reducer = omop_reduce("slope"))),
  outputs = omop_output("people", format = "wide", grain = "person"))
stopifnot(recipe_explain(slopes, conns = conns)$selected == "v2")
print(recipe_execute(slopes, out = c(people = "D_lab_slopes"), conns = conns))
```

    #>         people 
    #> "D_lab_slopes"

Summarise the slopes, not the private fitted lines.

``` r
slope_summary <- dsBaseClient::ds.summary("D_lab_slopes$per_day", datasources = conns)
knitr::kable(book_summary(slope_summary))
```

| Site | Statistic |       Value |
| :--- | :-------- | ----------: |
| site | 5%        | \-0.0007210 |
| site | 10%       | \-0.0006134 |
| site | 25%       | \-0.0001388 |
| site | 50%       |   0.0002210 |
| site | 75%       |   0.0004151 |
| site | 90%       |   0.0007838 |
| site | 95%       |   0.0011030 |
| site | Mean      |   0.0001429 |

**Exercise.** Express the slopes per 365 days and summarise them.

Solution

``` r
print(ds.omop.mutate("D_lab_slopes", per_365_days = 365 * per_day,
  newobj = "D_lab_annual", conns = conns))
```

    #> [1] "D_lab_annual"

``` r
annual_summary <- dsBaseClient::ds.summary("D_lab_annual$per_365_days", datasources = conns)
knitr::kable(book_summary(annual_summary))
```

| Site | Statistic |       Value |
| :--- | :-------- | ----------: |
| site | 5%        | \-0.2631522 |
| site | 10%       | \-0.2238866 |
| site | 25%       | \-0.0506511 |
| site | 50%       |   0.0806739 |
| site | 75%       |   0.1515094 |
| site | 90%       |   0.2860863 |
| site | 95%       |   0.4025790 |
| site | Mean      |   0.0521556 |

This rescales the same fitted slopes. It does not add observations or
forecast any person’s next result. The fixed 365-day interval is
explicit.

## Declare a common unit

To pool measurements, first align their meaning and units. Look up the
target unit grams per liter (g/L).

``` r
target_unit <- ds.omop.concept.lookup(8636L, conns = conns)
knitr::kable(target_unit$pooled["concept_name"])
```

| concept\_name  |
| :------------- |
| gram per liter |

Declare the conversion: g/dL multiplied by ten becomes g/L. Rows already
in g/L retain their value. Other units become missing by default; the
server never guesses a conversion. `unknown = "exclude"` would drop
those rows instead.

``` r
lab_gl <- omop_event("measurement", concepts = lab_id,
  unit = omop_unit(8636L, convert = list("8713" = c(factor = 10, offset = 0))))
class(lab_gl)
```

    #> [1] "omop_event" "list"

The conversion precedes predicates and reducers, and also transforms
recorded reference bounds. This source only has g/dL for this lab; we
are converting its actual values, not inventing a mixed-unit source.

``` r
converted <- omop_recipe(variables = list(mean_gl = omop_variable(event = lab_gl,
  reducer = omop_reduce("mean"))),
  outputs = omop_output("people", format = "wide", grain = "person"))
stopifnot(recipe_explain(converted, conns = conns)$selected == "v2")
print(recipe_execute(converted, out = c(people = "D_lab_gl"), conns = conns))
```

    #>     people 
    #> "D_lab_gl"

**Exercise.** Summarise the converted means. How should they compare
with the g/dL summary above?

Solution

``` r
conversion_summary <- dsBaseClient::ds.summary("D_lab_gl$mean_gl", datasources = conns)
knitr::kable(book_summary(conversion_summary))
```

| Site | Statistic |    Value |
| :--- | :-------- | -------: |
| site | 5%        | 129.0500 |
| site | 10%       | 131.4667 |
| site | 25%       | 137.8500 |
| site | 50%       | 143.2500 |
| site | 75%       | 153.3542 |
| site | 90%       | 157.0000 |
| site | 95%       | 159.2375 |
| site | Mean      | 144.6679 |

Every displayed quantile and the mean is ten times its earlier value.
Use the same target unit and declared conversion map at every site
before pooling.

## Form bins and high or low flags

A *bin* groups values into an interval. Apply it to each person’s mean,
privately. These are arbitrary teaching cutoffs, **not clinical
reference limits**.

``` r
print(ds.omop.mutate("D_labs",
  band = cut(mean, breaks = c(0, 14, 15, 100),
    labels = c("below 14", "14 to below 15", "15 or above"), right = FALSE),
  high = ifelse(mean >= 15, 1L, 0L), low = ifelse(mean < 14, 1L, 0L),
  newobj = "D_lab_flags", conns = conns))
```

    #> [1] "D_lab_flags"

Left boundaries are included; right boundaries are excluded. Values
outside the declared range and missing means have a missing bin. Release
the bin counts:

``` r
bin_counts <- dsBaseClient::ds.table("D_lab_flags$band", useNA = "always", datasources = conns)
knitr::kable(book_counts(bin_counts))
```

| Category       | Count |
| :------------- | ----: |
| 14 to below 15 |     9 |
| 15 or above    |    10 |
| below 14       |     9 |
| NA             |     0 |

These nonzero cells meet the release threshold. Smaller cells may cause
the whole table to be withheld. A refusal is not a zero count.

The dedicated `abnormal_high` and `abnormal_low` reducers instead
compare each reading with its own recorded `range_high` or `range_low`
and count readings outside the bounds. Synthea27Nj has **no recorded
reference bounds**. Run them to learn why a zero abnormal count does not
establish normal results.

``` r
ranges <- omop_recipe(variables = list(
  high_count = omop_variable(event = lab, reducer = omop_reduce("abnormal_high")),
  low_count = omop_variable(event = lab, reducer = omop_reduce("abnormal_low"))),
  outputs = omop_output("people", format = "wide", grain = "person"))
stopifnot(recipe_explain(ranges, conns = conns)$selected == "v2")
print(recipe_execute(ranges, out = c(people = "D_lab_ranges"), conns = conns))
```

    #>         people 
    #> "D_lab_ranges"

Turn those counts into person-level flags. Both summaries below show
zeros: there were no comparable bounds, so no recorded abnormalities
were counted.

``` r
print(ds.omop.mutate("D_lab_ranges",
  recorded_high = ifelse(high_count > 0, 1L, 0L),
  recorded_low = ifelse(low_count > 0, 1L, 0L),
  newobj = "D_recorded_flags", conns = conns))
```

    #> [1] "D_recorded_flags"

``` r
recorded_high <- dsBaseClient::ds.table("D_recorded_flags$recorded_high", useNA = "always", datasources = conns)
recorded_low <- dsBaseClient::ds.table("D_recorded_flags$recorded_low", useNA = "always", datasources = conns)
knitr::kable(rbind(transform(book_counts(recorded_high), Flag = "recorded high"),
                  transform(book_counts(recorded_low), Flag = "recorded low")))
```

| Category | Count | Flag          |
| :------- | ----: | :------------ |
| 0        |    28 | recorded high |
| NA       |     0 | recorded high |
| 0        |    28 | recorded low  |
| NA       |     0 | recorded low  |

**Exercise.** Summarise the two flags based on the explicit teaching
cutoffs. Why do they differ from the recorded-range flags?

Solution

``` r
high_counts <- dsBaseClient::ds.table("D_lab_flags$high", useNA = "always", datasources = conns)
low_counts <- dsBaseClient::ds.table("D_lab_flags$low", useNA = "always", datasources = conns)
knitr::kable(rbind(transform(book_counts(high_counts), Flag = "mean at least 15"),
                  transform(book_counts(low_counts), Flag = "mean below 14")))
```

| Category | Count | Flag             |
| :------- | ----: | :--------------- |
| 0        |    18 | mean at least 15 |
| 1        |    10 | mean at least 15 |
| NA       |     0 | mean at least 15 |
| 0        |    19 | mean below 14    |
| 1        |     9 | mean below 14    |
| NA       |     0 | mean below 14    |

The explicit flags compare a person’s mean with declared cutoffs. The
dedicated reducers compare individual readings with their own source
bounds, which are absent here. Neither result justifies a clinical
interpretation of these synthetic data.

## Calculate change from first to last

Define *change* as last minus first, in g/dL. Subtract on the server so
each pair belongs to the same person. This is a difference, not a rate
per day.

``` r
print(ds.omop.mutate("D_labs", change = last - first,
  newobj = "D_lab_change", conns = conns))
```

    #> [1] "D_lab_change"

A positive value means the last reading was larger. Request only its
summary:

``` r
change_summary <- dsBaseClient::ds.summary("D_lab_change$change", datasources = conns)
knitr::kable(book_summary(change_summary))
```

| Site | Statistic |       Value |
| :--- | :-------- | ----------: |
| site | 5%        | \-2.8950000 |
| site | 10%       | \-2.4900000 |
| site | 25%       | \-0.9500000 |
| site | 50%       |   0.9000000 |
| site | 75%       |   2.1750000 |
| site | 90%       |   3.2000000 |
| site | 95%       |   3.6550000 |
| site | Mean      |   0.9071429 |

With only one eligible reading, first and last coincide and change is
zero; that does not establish a stable trajectory. Missing endpoints
give missing change.

**Exercise.** Calculate each person’s observed spread, maximum minus
minimum. Is it the same as change?

Solution

``` r
print(ds.omop.mutate("D_labs", spread = maximum - minimum,
  newobj = "D_lab_spread", conns = conns))
```

    #> [1] "D_lab_spread"

``` r
spread_summary <- dsBaseClient::ds.summary("D_lab_spread$spread", datasources = conns)
knitr::kable(book_summary(spread_summary))
```

| Site | Statistic |    Value |
| :--- | :-------- | -------: |
| site | 5%        | 0.000000 |
| site | 10%       | 0.840000 |
| site | 25%       | 2.500000 |
| site | 50%       | 3.200000 |
| site | 75%       | 4.300000 |
| site | 90%       | 5.220000 |
| site | 95%       | 6.215000 |
| site | Mean      | 3.357143 |

Spread cannot be negative and ignores ordering. Change preserves the
direction between the first and last readings, but ignores the
intervening trajectory.

## Pool across two connections

This section is a **local DSLite exercise**, even if the earlier
sections used Opal. The helper creates temporary, disjoint copies of the
public Synthea data: 14 people in `north`, 14 in `south`. It preserves
their measurements and units, then logs into two actual DSLite servers.
It never partitions a live Opal dataset. A real multisite analysis
instead supplies its authorised connections and must establish that
populations do not overlap.

``` r
pair <- book_numeric_pair()
two_conns <- pair$conns
names(two_conns)
```

    #> [1] "north" "south"

Run the identical native recipe at both sites. A lab column has the same
meaning, unit, missing-value rule and person-level grain at each site.

``` r
stopifnot(recipe_explain(values, conns = two_conns, symbol = "book_pool")$selected == "v2")
print(recipe_execute(values, out = c(people = "D_pool_labs"), conns = two_conns, symbol = "book_pool"))
```

    #>        people 
    #> "D_pool_labs"

`ds.mean` combines nonmissing observations across sites. Here each
observation is one person’s mean lab value, so each person receives
equal weight.

``` r
pooled_mean <- dsBaseClient::ds.mean("D_pool_labs$mean", type = "combine", datasources = two_conns)
pooled_mean
```

    #> $Global.Mean
    #>                 EstimatedMean Nmissing Nvalid Ntotal
    #> studiesCombined      14.46679        0     28     28
    #> 
    #> $Nstudies
    #> [1] 2
    #> 
    #> $ValidityMessage
    #>       ValidityMessage 
    #> north "VALID ANALYSIS"
    #> south "VALID ANALYSIS"

`ds.quantileMean` averages site quantiles with site-size weights. It is
an estimate of the combined distribution, **not the exact quantiles of
concatenated patient values**. Its mean agrees with the pooled mean here
because all 28 person-level means are present. Missing data require care
with denominators.

``` r
pooled_quantiles <- dsBaseClient::ds.quantileMean("D_pool_labs$mean",
  type = "combine", datasources = two_conns)
knitr::kable(data.frame(Statistic = names(pooled_quantiles), Value = as.numeric(pooled_quantiles)))
```

| Statistic |    Value |
| :-------- | -------: |
| 5%        | 13.02050 |
| 10%       | 13.29000 |
| 25%       | 13.85104 |
| 50%       | 14.28917 |
| 75%       | 15.30625 |
| 90%       | 15.72500 |
| 95%       | 15.91000 |
| Mean      | 14.46679 |

A Gaussian *generalised linear model* (GLM) relates a numeric outcome to
a covariate. Fit last hemoglobin as the outcome and first hemoglobin as
the covariate, both in g/dL. The servers keep the person-level data; the
analyst sees model coefficients and diagnostics.

With the pinned dsBase 6.3.5, the call emits the warnings shown below:
its local setup fits use `maxit = 1`. The distributed algorithm then
iterates across sites; this example finishes in three iterations. The
book’s independent checks compare its final coefficients, standard
errors and deviance with a central Gaussian fit on the public synthetic
source. Do not ignore an unexplained warning in a new study.

``` r
pooled_model <- dsBaseClient::ds.glm(formula = "last ~ first", data = "D_pool_labs",
  family = "gaussian", datasources = two_conns)
```

    #> Warning: glm.fit: algorithm did not converge
    #> Warning: glm.fit: algorithm did not converge
    #> Warning: glm.fit: algorithm did not converge
    #> Warning: glm.fit: algorithm did not converge
    #> Warning: glm.fit: algorithm did not converge
    #> Warning: glm.fit: algorithm did not converge
    #> Warning: glm.fit: algorithm did not converge
    #> Warning: glm.fit: algorithm did not converge

``` r
knitr::kable(pooled_model$coefficients)
```

|             |    Estimate | Std. Error |     z-value |  p-value |  low0.95CI | high0.95CI |
| :---------- | ----------: | ---------: | ----------: | -------: | ---------: | ---------: |
| (Intercept) |  15.6082485 |  2.0915966 |   7.4623606 | 0.000000 |  11.508794 | 19.7077026 |
| first       | \-0.0511516 |  0.1479754 | \-0.3456763 | 0.729586 | \-0.341178 |  0.2388749 |

The `first` coefficient is the fitted difference in last hemoglobin for
a one-g/dL difference in first hemoglobin. The intercept is the
prediction at first = 0, far outside these data. This small synthetic
example teaches mechanics; it is not a causal result or a validated
prediction model.

Full model response

``` r
pooled_model
```

    #> $Nvalid
    #> [1] 28
    #> 
    #> $Nmissing
    #> [1] 0
    #> 
    #> $Ntotal
    #> [1] 28
    #> 
    #> $disclosure.risk
    #>       RISK OF DISCLOSURE
    #> north                  0
    #> south                  0
    #> 
    #> $errorMessage
    #>       ERROR MESSAGES
    #> north "No errors"   
    #> south "No errors"   
    #> 
    #> $nsubs
    #> [1] 28
    #> 
    #> $iter
    #> [1] 3
    #> 
    #> $family
    #> 
    #> Family: gaussian 
    #> Link function: identity 
    #> 
    #> 
    #> $formula
    #> [1] "last ~ first"
    #> 
    #> $coefficients
    #>                Estimate Std. Error    z-value      p-value low0.95CI high0.95CI
    #> (Intercept) 15.60824853  2.0915966  7.4623606 8.498588e-14 11.508794 19.7077026
    #> first       -0.05115158  0.1479754 -0.3456763 7.295860e-01 -0.341178  0.2388749
    #> 
    #> $dev
    #> [1] 66.81152
    #> 
    #> $df
    #> [1] 26
    #> 
    #> $output.information
    #> [1] "SEE TOP OF OUTPUT FOR INFORMATION ON MISSING DATA AND ERROR MESSAGES"

**Exercise 1.** Pool the first-value means. Would opening two
connections to the same 28 people be a valid way to increase the sample
size?

Solution 1

``` r
pooled_first <- dsBaseClient::ds.mean("D_pool_labs$first", type = "combine", datasources = two_conns)
pooled_first
```

    #> $Global.Mean
    #>                 EstimatedMean Nmissing Nvalid Ntotal
    #> studiesCombined      13.98571        0     28     28
    #> 
    #> $Nstudies
    #> [1] 2
    #> 
    #> $ValidityMessage
    #>       ValidityMessage 
    #> north "VALID ANALYSIS"
    #> south "VALID ANALYSIS"

No. Duplicate people would be counted twice, and model uncertainty would
be misleading. These temporary sites instead have disjoint people.

**Exercise 2.** Is the mean of the two site medians necessarily the
median you would get from all 28 people together?

Solution 2

No. These sites are equally sized, so `ds.quantileMean` averages their
medians equally. In general that differs from the median of the combined
observations; with unequal site sizes the function uses unequal weights.

## Recap

Start with a lab meaning and its recorded units. Choose the person-level
reducer and time window before releasing summaries. Declare conversions,
distinguish teaching cutoffs from recorded reference bounds, and
distinguish change from slope. Pool only compatible variables from
nonoverlapping sites, and interpret `ds.quantileMean` according to its
weighted-site definition.
