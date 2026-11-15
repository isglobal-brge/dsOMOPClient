# 14\. Working with lab values

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

This chapter uses **synthetic Synthea27Nj measurements**, described in
[Getting
started](https://isglobal-brge.github.io/dsOMOPClient/articles/book-00-start.html#load-two-public-synthetic-datasets).

Concept selections refer to the searches and saved definitions in
[chapter 4](https://isglobal-brge.github.io/dsOMOPClient/articles/book-04-vocabulary.md),
including the separate questionnaire catalogue. Additional
body-measurement concepts are found on screen in the [formula
walkthrough](https://isglobal-brge.github.io/dsOMOPClient/articles/book-15-formulas.html#find-the-inputs-before-calculating).

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
hits <- ds.omop.concept.search("Hemoglobin", domain = "Measurement", conns = conns)
knitr::kable(head(hits$pooled[c("concept_name", "domain_id", "vocabulary_id")], 6))
```

| concept\_name                                                                        | domain\_id  | vocabulary\_id |
| :----------------------------------------------------------------------------------- | :---------- | :------------- |
| Hemoglobin and Hematocrit panel - Blood                                              | Measurement | LOINC          |
| Hemoglobin A1c/Hemoglobin.total in Blood                                             | Measurement | LOINC          |
| Hemoglobin.gastrointestinal.lower \[Presence\] in Stool by Immunoassay –1st specimen | Measurement | LOINC          |
| Hemoglobin \[Presence\] in Urine by Test strip                                       | Measurement | LOINC          |
| Hemoglobin \[Mass/volume\] in Blood                                                  | Measurement | LOINC          |

Select the blood mass-concentration concept deliberately. Other
hemoglobin concepts can describe a different test. IDs in code select
public vocabulary; shown tables use their names.

``` r
lab_id <- hits$pooled$concept_id[hits$pooled$concept_name == "Hemoglobin [Mass/volume] in Blood"]
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
The recorded unit is grams per deciliter (g/dL). The analyst sees the
protected aggregate above.

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
  unit = omop_unit(8713L))
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
book_symbols(recipe_execute(values, out = c(people = "D_labs"), conns = conns))
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

## Compare reducers and their groupings

Keep the raw number and its grouping together. Use the same teaching
threshold for first and last; grouping hides the size of differences
within each category. The repeated mean in each row below is the
**overall mean of that raw column**, not a category-specific mean. Only
released aggregates are combined locally.

``` r
grouped <- omop_recipe(variables = list(first = first, last = last,
  first_group = omop_number_bins(first, breaks = c(-Inf, 14.5, Inf),
    labels = c("below 14.5", "14.5 or above")),
  last_group = omop_number_bins(last, breaks = c(-Inf, 14.5, Inf),
    labels = c("below 14.5", "14.5 or above"))))
book_symbols(recipe_execute(grouped, out = c(output = "D_reducer_groups"), conns = conns))
```

    #>             output 
    #> "D_reducer_groups"

``` r
first_groups <- dsBaseClient::ds.table("D_reducer_groups$first_group", datasources = conns)
first_mean <- dsBaseClient::ds.mean("D_reducer_groups$first", type = "combine", datasources = conns)
last_groups <- dsBaseClient::ds.table("D_reducer_groups$last_group", datasources = conns)
last_mean <- dsBaseClient::ds.mean("D_reducer_groups$last", type = "combine", datasources = conns)
knitr::kable(rbind(book_group_comparison(first_groups, first_mean, "first", "g/dL"),
                  book_group_comparison(last_groups, last_mean, "last", "g/dL")))
```

| Category      | Count | Number | Unit |     Mean |
| :------------ | ----: | :----- | :--- | -------: |
| below 14.5    |    16 | first  | g/dL | 13.98571 |
| 14.5 or above |    12 | first  | g/dL | 13.98571 |
| NA            |     0 | first  | g/dL | 13.98571 |
| below 14.5    |    10 | last   | g/dL | 14.89286 |
| 14.5 or above |    18 | last   | g/dL | 14.89286 |
| NA            |     0 | last   | g/dL | 14.89286 |

**Exercise.** Group the per-person mean using the same cutoff and show
its raw mean.

Solution

``` r
grouped_mean <- omop_recipe(variables = list(mean = average,
  mean_group = omop_number_bins(average, breaks = c(-Inf, 14.5, Inf),
    labels = c("below 14.5", "14.5 or above"))))
book_symbols(recipe_execute(grouped_mean, out = c(output = "D_mean_groups"), conns = conns))
```

    #>          output 
    #> "D_mean_groups"

``` r
mean_groups <- dsBaseClient::ds.table("D_mean_groups$mean_group", datasources = conns)
raw_mean <- dsBaseClient::ds.mean("D_mean_groups$mean", type = "combine", datasources = conns)
knitr::kable(book_group_comparison(mean_groups, raw_mean, "person mean", "g/dL"))
```

| Category      | Count | Number      | Unit |     Mean |
| :------------ | ----: | :---------- | :--- | -------: |
| below 14.5    |    15 | person mean | g/dL | 14.46679 |
| 14.5 or above |    13 | person mean | g/dL | 14.46679 |
| NA            |     0 | person mean | g/dL | 14.46679 |

Every person still contributes once; reducing their readings and then
grouping the result answers a different question from grouping each
measurement first.

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
book_symbols(recipe_execute(indexed, out = c(baseline = "D_lab_index"), conns = conns))
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
book_symbols(recipe_execute(slopes, out = c(people = "D_lab_slopes"), conns = conns))
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
annual <- omop_recipe(variables = list(per_365_days = omop_number(
  omop_number_change(lab, "slope", time_unit = "days"),
  omop_number_spec(transform = omop_number_transform("affine", factor = 365)))))
book_symbols(recipe_execute(annual, out = c(output = "D_lab_annual"), conns = conns))
```

    #>         output 
    #> "D_lab_annual"

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
book_symbols(recipe_execute(converted, out = c(people = "D_lab_gl"), conns = conns))
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

## Round and bin without losing the raw number

Keep a raw mean, a rounded mean and a band from the same declaration.
Native bins are **ordered factors** on the server, with every declared
level retained. The mean is reduced before rounding and binning; these
are public teaching cutoffs, not clinical reference limits.

``` r
band_levels <- c("below 14", "14 to below 15", "15 or above")
flags <- omop_recipe(variables = list(mean = average,
  rounded = omop_number(average, omop_number_spec(rounding = omop_number_round("decimals", 1))),
  band = omop_number_bins(average, breaks = c(-Inf, 14, 15, Inf), labels = band_levels),
  high = omop_number_flag(average, "ge", 15),
  low = omop_number_flag(average, "lt", 14)))
book_symbols(recipe_execute(flags, out = c(output = "D_lab_flags"), conns = conns))
```

    #>        output 
    #> "D_lab_flags"

``` r
bin_counts <- dsBaseClient::ds.table("D_lab_flags$band", useNA = "always", datasources = conns)
knitr::kable(book_counts(bin_counts))
```

| Category       | Count |
| :------------- | ----: |
| below 14       |     9 |
| 14 to below 15 |     9 |
| 15 or above    |    10 |
| NA             |     0 |

Unlike the old mutate-plus-cut workaround, the factor is ordered before
release. That affects model contrasts: choose the raw number or explicit
contrasts when a linear ordinal effect is not the model you intend.

**Exercise (basic).** Check the factor type and summarise the rounded
number.

Solution: native types

``` r
dsBaseClient::ds.class("D_lab_flags$band", datasources = conns)
```

    #> $site
    #> [1] "dsomop_date_factor" "ordered"            "factor"

``` r
knitr::kable(book_summary(dsBaseClient::ds.summary("D_lab_flags$rounded", datasources = conns)))
```

| Site | Statistic |    Value |
| :--- | :-------- | -------: |
| site | 5%        | 12.90500 |
| site | 10%       | 13.17000 |
| site | 25%       | 13.77500 |
| site | 50%       | 14.30000 |
| site | 75%       | 15.32500 |
| site | 90%       | 15.69000 |
| site | 95%       | 15.96500 |
| site | Mean      | 14.45714 |

The type check belongs here because this lesson is about types. Rounding
the person means does not guarantee that their pooled mean has one
decimal place.

## Use low, normal and high explicitly

For an aggregate such as a mean, supply public thresholds. “Normal” here
only means the middle interval of this teaching rule. It is not a
diagnosis.

``` r
category_recipe <- omop_recipe(variables = list(control =
  omop_number_category(average, range = "thresholds", thresholds = c(14, 15))))
book_symbols(recipe_execute(category_recipe, out = c(output = "D_control"), conns = conns))
```

    #>      output 
    #> "D_control"

``` r
knitr::kable(book_counts(dsBaseClient::ds.table("D_control$control", datasources = conns)))
```

| Category | Count |
| :------- | ----: |
| low      |     9 |
| normal   |     9 |
| high     |    10 |
| NA       |     0 |

Reference mode instead uses the bounds paired with the selected
observation. Synthea27Nj records no bounds for haemoglobin, so this
example deliberately shows the source-validation refusal for missing
reference bounds. The missing bounds cannot establish normality. Do not
invent bounds or weaken the release check to obtain categories. The
declared-threshold example above is the runnable alternative when those
cutoffs are part of the protocol.

The refusal can also emit the DSLite cleanup warning shown below: an
internal execution handle has already been removed. It is a cleanup
diagnostic, not a released patient count or a successful assignment. Its
temporary handle name changes between sessions. Both diagnostics remain
visible in this lesson.

``` r
reference_recipe <- omop_recipe(variables = list(control = omop_number_category(last, range = "reference")))
reference_message <- tryCatch({
  recipe_execute(reference_recipe, out = c(output = "D_reference"), conns = conns)
  "Reference output assigned"
}, error = function(e) conditionMessage(e))
```

    #> Warning in rm(list = c(name), envir = private$.session(sid)): object 'dsOexec.tHBajA'
    #> not found

``` r
cat(reference_message, "\n")
```

    #> Plan publication refused: site:output [validation_error] (number_source_invalid). Execution-owned artifacts were revoked and removed.

**Exercise (intermediate).** Count people whose mean is at least 15
g/dL.

Solution: threshold flag

``` r
knitr::kable(book_counts(dsBaseClient::ds.table("D_lab_flags$high", datasources = conns)))
```

| Category | Count |
| :------- | ----: |
| 0        |    18 |
| 1        |    10 |
| NA       |     0 |

The flag is a numeric 0/1 value with missing inputs preserved. Its
threshold belongs to the coordinate being classified, here g/dL after
reduction.

## Compose conversion, rounding and classification

A pipeline fixes the order. Convert g/dL to g/L, round to one decimal,
then classify using cutoffs in **g/L**. Rounding can move a value across
a cutoff; classifying before rounding answers a different question.

``` r
presentation <- omop_number_pipeline(
  omop_number_unit("unit:8713", "unit:8636", factor = 10),
  omop_number_round("decimals", 1),
  omop_number_spec(representation = "bins", breaks = c(0, 140, 150, 1000),
    labels = c("below 140", "140 to below 150", "150 or above")))
pipeline_recipe <- omop_recipe(variables = list(band = omop_number(average, presentation)))
explanation <- recipe_explain(pipeline_recipe, detail = "semantics")
cat(grep("numeric|unit:|round|pipeline|levels", explanation$text, value = TRUE), sep = "\n")
```

    #> Federation: permitted ID union, including declared unused levels; newest comparable vocabulary version, then bytewise name tie. Unresolved versions never establish recency.
    #> Precedence: variable/wrapper > recipe > design; pipelines are atomic. Canonical inputs bypass transforms/rounds/categories and outer recipe defaults.
    #> Public bins/categories retain full ordered levels and polynomial contrasts; quantitative trends require a numeric sibling. Reference categories are relative to each selected observation's paired local ranges.
    #> Required numeric capabilities: number.presentation
    #> Numeric variable 'band': {"broadcast":false,"column_names":{},"labels":{},"number_spec":{"pipeline":[{"factor":10,"from":"unit:8713","kind":"fixed_unit","offset":0,"to":"unit:8636"},{"digits":1,"kind":"round","method":"decimals"},{"breaks":[0,140,150,1000],"closed":"left","kind":"bins","labels":["below 140","140 to below 150","150 or above"]}]},"reducer":{"agg":null,"coverage":"recorded","distinct":null,"fills":"start_in_window","fn":"mean","missing":"skip","n":1,"observed_only":false,"p":null,"predicate":null,"reference":null,"reference_date":null,"side":"either","unit":"day","value":"value","which":null,"window":null},"source":{"event":"lab_3000963"}}; source/reducer grain retained; broadcast=FALSE; date/class-2 lineage never declassified.
    #>   specification origin: variable/wrapper; explicit declaration: {"pipeline":[{"factor":10,"from":"unit:8713","kind":"fixed_unit","offset":0,"to":"unit:8636"},{"digits":1,"kind":"round","method":"decimals"},{"breaks":[0,140,150,1000],"kind":"bins","labels":["below 140","140 to below 150","150 or above"]}]}
    #>   canonical source coordinate: unit:8713
    #>   stage 1 fixed_unit: numeric -> numeric; coordinate unit:8636; private scalar presentation before sealing
    #>   stage 2 round: numeric -> numeric; coordinate unit:8636; private scalar presentation before sealing
    #>   stage 3 bins: numeric -> ordered factor (contr.poly); coordinate unit:8636; complete public levels: below 140 | 140 to below 150 | 150 or above; private scalar presentation before sealing

Read the declared coordinate, stage order and complete public level set.
No private distribution is needed to explain a public presentation rule.

``` r
book_symbols(recipe_execute(pipeline_recipe, out = c(output = "D_pipeline"), conns = conns))
```

    #>       output 
    #> "D_pipeline"

``` r
knitr::kable(book_counts(dsBaseClient::ds.table("D_pipeline$band", datasources = conns)))
```

| Category         | Count |
| :--------------- | ----: |
| below 140        |     9 |
| 140 to below 150 |     9 |
| 150 or above     |    10 |
| NA               |     0 |

**Exercise (advanced).** Make a logarithmic mean column, preserving the
raw mean.

Solution: transform

``` r
log_recipe <- omop_recipe(variables = list(raw = average,
  logged = omop_number(average, omop_number_spec(transform = omop_number_transform("log")))))
book_symbols(recipe_execute(log_recipe, out = c(output = "D_log"), conns = conns))
```

    #>  output 
    #> "D_log"

``` r
knitr::kable(book_summary(dsBaseClient::ds.summary("D_log$logged", datasources = conns)))
```

| Site | Statistic |    Value |
| :--- | :-------- | -------: |
| site | 5%        | 2.557554 |
| site | 10%       | 2.576166 |
| site | 25%       | 2.623579 |
| site | 50%       | 2.661968 |
| site | 75%       | 2.730164 |
| site | 90%       | 2.753634 |
| site | 95%       | 2.767809 |
| site | Mean      | 2.669345 |

This is log of each person’s mean, not mean of their logged
observations. Invalid transform inputs remain missing. Specify the
scientific reason for transforming before fitting a model.

## Change over time and first observed attainment

Difference is last minus first. Percent change divides that difference
by first; ratio divides last by first. A zero baseline cannot support
either division. Slope uses actual elapsed time. Time to threshold is
first **observed** attainment, without interpolation; missing attainment
is not evidence of permanent failure.

``` r
changes <- omop_recipe(variables = list(
  change = omop_number_change(lab, "difference"),
  percent = omop_number_change(lab, "percent_change"),
  ratio = omop_number_change(lab, "ratio"),
  per_day = omop_number_change(lab, "slope", time_unit = "days"),
  target_days = omop_number_change(lab, "time_to_threshold", threshold = 14,
    op = "ge", reference = "first", time_unit = "days")))
book_symbols(recipe_execute(changes, out = c(output = "D_lab_change"), conns = conns))
```

    #>         output 
    #> "D_lab_change"

``` r
change_summary <- dsBaseClient::ds.summary("D_lab_change$change", datasources = conns)
knitr::kable(book_summary(change_summary))
```

| Site | Statistic |   Value |
| :--- | :-------- | ------: |
| site | 5%        | \-2.940 |
| site | 10%       | \-2.580 |
| site | 25%       | \-1.400 |
| site | 50%       |   1.600 |
| site | 75%       |   2.400 |
| site | 90%       |   3.200 |
| site | 95%       |   3.760 |
| site | Mean      |   1.016 |

A single observed value produces missing native change: two eligible
readings are required. Subtracting separately reduced first and last
would instead give zero for that person, which would falsely suggest an
observed change. Measurement frequency affects observed attainment
times.

**Exercise.** Summarise the time to the teaching target. What does day
zero mean?

Solution: observed target

``` r
knitr::kable(book_summary(dsBaseClient::ds.summary("D_lab_change$target_days", datasources = conns)))
```

| Site | Statistic |    Value |
| :--- | :-------- | -------: |
| site | 5%        |    0.000 |
| site | 10%       |    0.000 |
| site | 25%       |    0.000 |
| site | 50%       |  185.500 |
| site | 75%       | 2192.750 |
| site | 90%       | 3052.000 |
| site | 95%       | 4166.750 |
| site | Mean      | 1274.808 |

Day zero means the first eligible reading already met the threshold. A
person who never attains it has a missing time; use an explicit censored
survival design for a time-to-event analysis rather than dropping those
people.

## Several columns from one output value

Output-column declarations copy a numeric value privately with its row
identity and privacy lineage. They do not join unrelated measurement
rows.

``` r
column_recipe <- omop_recipe(variables = list(mean = average),
  outputs = omop_output("people", format = "wide", grain = "person", columns = c("mean", "mean_rounded"),
    number_columns = list(mean_rounded = omop_number_column("mean",
      number_spec = omop_number_spec(rounding = omop_number_round("decimals", 1))))))
book_symbols(recipe_execute(column_recipe, out = c(people = "D_number_columns"), conns = conns))
```

    #>             people 
    #> "D_number_columns"

``` r
dsBaseClient::ds.colnames("D_number_columns", datasources = conns)
```

    #> $site
    #> [1] "person_id"    "mean"         "mean_rounded"

The stable input key is `mean`, before presentation renames. Canonical
numeric inputs bypass display settings; explicitly presented inputs must
still be numeric. An ordered bin cannot be fed back into arithmetic as
if it were the original value.

**Exercise.** Verify that the copied rounded column has the same summary
as the earlier rounded column.

Solution: column copy

``` r
knitr::kable(book_summary(dsBaseClient::ds.summary("D_number_columns$mean_rounded", datasources = conns)))
```

| Site | Statistic |    Value |
| :--- | :-------- | -------: |
| site | 5%        | 12.90500 |
| site | 10%       | 13.17000 |
| site | 25%       | 13.77500 |
| site | 50%       | 14.30000 |
| site | 75%       | 15.32500 |
| site | 90%       | 15.69000 |
| site | 95%       | 15.96500 |
| site | Mean      | 14.45714 |

Compare this aggregate with the native-types solution above. The row
grain and rounding declaration are the same.

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
book_symbols(recipe_execute(values, out = c(people = "D_pool_labs"), conns = two_conns, symbol = "book_pool"))
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

With dsBase 6.3.5, the call emits the warnings shown below: its local
setup fits use `maxit = 1`. The distributed algorithm then iterates
across sites; this example finishes in three iterations. The book’s
independent checks compare its final coefficients, standard errors and
deviance with a central Gaussian fit on the public synthetic source. Do
not ignore an unexplained warning in a new study.

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

**Next:** [15. Derived numeric
formulas](https://isglobal-brge.github.io/dsOMOPClient/articles/book-15-formulas.md).
