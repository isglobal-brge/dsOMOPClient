# 14\. Working with lab values

**What you will learn**

Find a lab and its units, reduce repeated readings, present numbers
without losing their meaning, and fit a model that requires an actual
follow-up measurement.

**You need:** the Opal login from [Getting
started](https://isglobal-brge.github.io/dsOMOPClient/articles/book-00-start.md)
and [events and
reducers](https://isglobal-brge.github.io/dsOMOPClient/articles/book-06-events.md).
This chapter uses `omop.synthea`, then its two-site split.

``` r
library(dsOMOPClient)
conns <- ds.omop.login("https://localhost:8443", user = "administrator",
  password = "password", resource = "omop.synthea", server = "site")$conns
```

## Find haemoglobin and its unit

``` r
hits <- ds.omop.concept.search("Hemoglobin", domain = "Measurement", conns = conns)$pooled
hits[c("concept_id", "concept_name", "standard_concept")]
```

| concept\_id | concept\_name                                                                         | standard\_concept |
| ----------: | :------------------------------------------------------------------------------------ | :---------------- |
|     3001643 | Hemoglobin and Hematocrit panel - Blood                                               | S                 |
|     3004410 | Hemoglobin A1c/Hemoglobin.total in Blood                                              | S                 |
|    40761014 | Hemoglobin.gastrointestinal.lower \[Presence\] in Stool by Immunoassay --1st specimen | S                 |
|     3011397 | Hemoglobin \[Presence\] in Urine by Test strip                                        | S                 |
|     3000963 | Hemoglobin \[Mass/volume\] in Blood                                                   | S                 |

``` r
lab_id <- hits$concept_id[trimws(hits$concept_name) == "Hemoglobin [Mass/volume] in Blood"]
units <- ds.omop.value.counts("measurement", "unit_concept_id", concept_id = lab_id,
  scope = "pooled", conns = conns)$pooled
units[c("value", "concept_name", "n")]
```

| value | concept\_name      |   n |
| :---- | :----------------- | --: |
| 8713  | gram per deciliter | 105 |

Choose **3000963**, Hemoglobin \[Mass/volume\] in Blood. The recorded
unit is **8713**, gram per deciliter (g/dL); `value` holds that unit ID,
and `n` is a record count rounded down to a multiple of five. Other
haemoglobin concepts need not measure the same quantity.

## Choose what one person’s number means

A unit declaration preserves the chosen unit. With the default
unknown-unit rule, other-unit rows remain but their numeric values
become missing; there is no automatic conversion. First and last use
dates, while min and max use magnitude. All-history mean gives each
reading equal weight within a person.

``` r
lab <- omop_event("measurement", concepts = lab_id, unit = omop_unit(8713L))
first <- omop_variable(event = lab, reducer = omop_reduce("first_value"))
last <- omop_variable(event = lab, reducer = omop_reduce("last_value"))
average <- omop_variable(event = lab, reducer = omop_reduce("mean"))
values <- omop_recipe(variables = list(first = first, last = last, mean = average,
  minimum = omop_variable(event = lab, reducer = omop_reduce("min")),
  maximum = omop_variable(event = lab, reducer = omop_reduce("max"))))
recipe_execute(values, out = "D_labs", conns = conns)
reducer_means <- do.call(rbind, lapply(c("first", "last", "mean", "minimum", "maximum"), function(column) {
  x <- dsBaseClient::ds.mean(paste0("D_labs$", column), type = "combine", datasources = conns)$Global.Mean
  data.frame(Reducer = column, Mean_g_dl = x[1, "EstimatedMean"],
    Valid = x[1, "Nvalid"], Missing = x[1, "Nmissing"])
}))
reducer_means
```

| Reducer | Mean\_g\_dl | Valid | Missing |
| :------ | ----------: | ----: | ------: |
| first   |      13.986 |    28 |       0 |
| last    |      14.893 |    28 |       0 |
| mean    |      14.467 |    28 |       0 |
| minimum |      12.746 |    28 |       0 |
| maximum |      16.104 |    28 |       0 |

The first-value mean is 13.986 g/dL and the last-value mean 14.893 g/dL.
The mean of person means is 14.467 g/dL: each of the 28 people
contributes once, regardless of how many readings they have. These first
and last means use the same 28 people, so their difference, 0.907 g/dL,
is their mean last-minus-first change. Three people have only one
reading. The change reducer excludes those three and gives 1.016 g/dL
among the 25 people with repeat readings: 0.907 × 28 / 25 (using the
unrounded difference).

## Keep numbers beside their groups

Grouping first and last values at the same cutoff helps compare
distributions. Each row gives the number of people and the mean within
that category.

``` r
grouped <- omop_recipe(variables = list(first = first, last = last,
  first_group = omop_number_bins(first, breaks = c(-Inf, 14.5, Inf),
    labels = c("below 14.5", "14.5 or above")),
  last_group = omop_number_bins(last, breaks = c(-Inf, 14.5, Inf),
    labels = c("below 14.5", "14.5 or above"))))
recipe_execute(grouped, out = "D_groups", conns = conns)
comparison <- lapply(c("first", "last"), function(column) {
  groups <- dsBaseClient::ds.meanSdGp(paste0("D_groups$", column),
    paste0("D_groups$", column, "_group"), type = "combine", datasources = conns)
  data.frame(Number = column, Category = c("below 14.5", "14.5 or above"),
    People = as.numeric(groups$Nvalid_gp), Mean_g_dl = as.numeric(groups$Mean_gp))
})
do.call(rbind, comparison)
```

| Number | Category      | People | Mean\_g\_dl |
| :----- | :------------ | -----: | ----------: |
| first  | below 14.5    |     16 |      12.650 |
| first  | 14.5 or above |     12 |      15.767 |
| last   | below 14.5    |     10 |      13.120 |
| last   | 14.5 or above |     18 |      15.878 |

Read People as each mean’s denominator: one first or last reading per
person within that category. Each pair of categories accounts for all 28
people. These cutoffs illustrate presentation; they are not clinical
reference limits.

## Select a nearby value and measure change

Index seven days after first haemoglobin and look back seven days.
Everyone has a reading in that window by construction; the nearest
reading can still differ from the first.

``` r
indexed <- omop_study(omop_index(lab, offset = 7L), outputs = "baseline",
  baseline = list(at_index = omop_value_at_index("measurement", lab_id,
    tolerance = c(-7, 0), unit = omop_unit(8713L))))
recipe_execute(indexed, out = c(baseline = "D_index"), conns = conns)
dsBaseClient::ds.summary("D_index$at_index", datasources = conns)
```

    #> $site
    #> $site$class
    #> [1] "numeric"
    #>
    #> $site$length
    #> [1] 28
    #>
    #> $site$`quantiles & mean`
    #>     5%    10%    25%    50%    75%    90%    95%   Mean
    #> 11.810 12.480 12.975 13.700 15.425 16.190 16.855 14.150

``` r
dsBaseClient::ds.numNA("D_index$at_index", datasources = conns)
```

    #> $site
    #> [1] 0

All 28 values are present. The mean becomes 14.150 g/dL because one
person’s next-day reading, 11.6, replaces their first reading, 7.0.
Those values belong to the public synthetic example; in a study, the
protected summary alone would not identify the changed person. A
narrower tolerance could leave missing values.

A slope fits value against actual date within each person. Difference is
last minus first. Two eligible readings are required for change, and a
slope needs distinct dates.

``` r
changes <- omop_recipe(variables = list(
  change = omop_number_change(lab, "difference"),
  per_year = omop_number(omop_number_change(lab, "slope", time_unit = "days"),
    omop_number_spec(transform = omop_number_transform("affine", factor = 365))),
  target_days = omop_number_change(lab, "time_to_threshold", threshold = 14,
    op = "ge", reference = "first", time_unit = "days")))
recipe_execute(changes, out = "D_change", conns = conns)
dsBaseClient::ds.summary("D_change$change", datasources = conns)
```

    #> $site
    #> $site$class
    #> [1] "numeric"
    #>
    #> $site$length
    #> [1] 28
    #>
    #> $site$`quantiles & mean`
    #>     5%    10%    25%    50%    75%    90%    95%   Mean
    #> -2.940 -2.580 -1.400  1.600  2.400  3.200  3.760  1.016

``` r
dsBaseClient::ds.numNA("D_change$change", datasources = conns)
```

    #> $site
    #> [1] 3

``` r
dsBaseClient::ds.summary("D_change$per_year", datasources = conns)
```

    #> $site
    #> $site$class
    #> [1] "numeric"
    #>
    #> $site$length
    #> [1] 28
    #>
    #> $site$`quantiles & mean`
    #>          5%         10%         25%         50%         75%         90%         95%
    #> -0.26315218 -0.22388661 -0.05065106  0.08067385  0.15150943  0.28608626  0.40257901
    #>        Mean
    #>  0.05215555

There are 25 valid changes and three missing: single-reading people do
not have observed change. Mean change is 1.016 g/dL. The median slope is
about 0.081 g/dL per 365 days. Neither statistic implies the same change
for every person, and a linear slope does not forecast a person’s next
reading.

## Convert, round and classify in a declared order

Target unit **8636** is gram per liter (g/L), found in
[chapter 6](https://isglobal-brge.github.io/dsOMOPClient/articles/book-06-events.html#convert-to-a-common-unit-before-reducing).
Multiplication by ten converts g/dL to g/L. Here the conversion is
applied to the person mean; an event-level `omop_unit(8636L, convert =
list("8713" = c(factor = 10, offset = 0)))` would convert before
reduction instead.

``` r
band_labels <- c("below 14", "14 to below 15", "15 or above")
presentation <- omop_number_pipeline(
  omop_number_unit("unit:8713", "unit:8636", factor = 10),
  omop_number_round("decimals", 1),
  omop_number_spec(representation = "bins", breaks = c(0, 140, 150, 1000),
    labels = c("below 140", "140 to below 150", "150 or above")))
flags <- omop_recipe(variables = list(mean = average,
  rounded = omop_number(average, omop_number_spec(rounding = omop_number_round("decimals", 1))),
  band = omop_number_bins(average, breaks = c(-Inf, 14, 15, Inf), labels = band_labels),
  converted_band = omop_number(average, presentation),
  control = omop_number_category(average, range = "thresholds", thresholds = c(14, 15)),
  high = omop_number_flag(average, "ge", 15)))
recipe_execute(flags, out = "D_flags", conns = conns)
band_means <- dsBaseClient::ds.meanSdGp("D_flags$mean", "D_flags$band",
  type = "combine", datasources = conns)
data.frame(Band = band_labels, People = as.numeric(band_means$Nvalid_gp),
  Mean_g_dl = as.numeric(band_means$Mean_gp))
```

| Band           | People | Mean\_g\_dl |
| :------------- | -----: | ----------: |
| below 14       |      9 |      13.341 |
| 14 to below 15 |      9 |      14.325 |
| 15 or above    |     10 |      15.607 |

``` r
dsBaseClient::ds.table("D_flags$converted_band",
  datasources = conns)$output.list$TABLES.COMBINED_all.sources_counts
```

    #> D_flags$converted_band
    #>        below 140 140 to below 150     150 or above               NA
    #>                9                9               10                0

Both presentations give category counts 9, 9 and 10 here. The
within-band means are 13.341, 14.325 and 15.607 g/dL: each person
contributes their all-history mean once to their band. Bins are
left-closed: 14 belongs to the middle bin and 15 to the upper bin. The
bands are an ordered factor. In a model, R reports linear/quadratic
terms (`.L`, `.Q`) instead of one coefficient per band. The fitted
values are the same, but read the coefficients accordingly.

The equal counts do not mean rounding can never change a category.
Consider a public example near the boundary:

``` r
x <- 13.996
data.frame(Input_g_dl = x, Converted_g_l = 10 * x,
  Rounded_g_l = round(10 * x, 1), Before = "below 140", After = "140 to below 150")
```

| Input\_g\_dl | Converted\_g\_l | Rounded\_g\_l | Before    | After            |
| -----------: | --------------: | ------------: | :-------- | :--------------- |
|       13.996 |          139.96 |           140 | below 140 | 140 to below 150 |

139.96 rounds to 140.0 and crosses the cutoff. At an exact halfway
value, rounding chooses an even final retained digit: 140.25 rounds to
140.2 at one decimal, whereas 140.75 rounds to 140.8. Classifying before
rounding would answer a different question.

``` r
dsBaseClient::ds.table("D_flags$control",
  datasources = conns)$output.list$TABLES.COMBINED_all.sources_counts
```

    #> D_flags$control
    #>    low normal   high     NA
    #>      9      9     10      0

``` r
dsBaseClient::ds.table("D_flags$high",
  datasources = conns)$output.list$TABLES.COMBINED_all.sources_counts
```

    #> D_flags$high
    #>  0  1 NA
    #> 18 10  0

The high flag is one for 10 people. The category thresholds define low
\<14, normal 14–15 **inclusive**, and high \>15, whereas the flag uses
≥15. Thus a value exactly 15 would be normal in the category but high
in the flag. Synthea has no haemoglobin reference bounds; missing bounds
cannot establish normality.

## Pool two disjoint sites

A last value equal to first because no second reading exists is not a
follow-up outcome. Index on the first lab and restrict last to days 1
onward. Use the two Synthea sites, which hold disjoint halves of the
same 28 people. Log in to both as a second connection, `two_conns`. Its
handle is named `pair` because `omop` is already attached through
`conns`, whose tables the exercises below still use.

``` r
two_conns <- ds.omop.login(rep("https://localhost:8443", 2), user = "administrator",
  password = "password",
  server = c("north", "south"),
  resource = c(north = "omop.synthea_north", south = "omop.synthea_south"),
  symbol = "pair")$conns
```

``` r
followup <- omop_study(lab, outputs = "baseline", baseline = list(
  first = omop_value_at_index("measurement", lab_id, tolerance = c(0, 0), unit = omop_unit(8713L)),
  last = omop_variable(event = lab, reducer = omop_reduce("last_value", window = omop_window(1, NULL))),
  followup_days = omop_variable(event = lab, reducer = omop_reduce("last_date", window = omop_window(1, NULL))),
  mean = average))
recipe_execute(followup, out = c(baseline = "D_pool"), conns = two_conns, symbol = "pair")
pooled_mean <- dsBaseClient::ds.mean("D_pool$mean", type = "combine", datasources = two_conns)$Global.Mean
pooled_mean
```

|                 | EstimatedMean | Nmissing | Nvalid | Ntotal |
| :-------------- | ------------: | -------: | -----: | -----: |
| studiesCombined |        14.467 |        0 |     28 |     28 |

The pooled mean remains 14.467 g/dL across 28 people. Each nonmissing
person receives equal weight; opening two connections to the same people
would double-count them.

``` r
dsBaseClient::ds.quantileMean("D_pool$mean", type = "combine", datasources = two_conns)
```

    #>       5%      10%      25%      50%      75%      90%      95%     Mean
    #> 13.02050 13.29000 13.85104 14.28917 15.30625 15.72500 15.91000 14.46679

These are weighted averages of site quantiles, not exact quantiles of
all people combined. The mean always agrees with the pooled mean because
this function weights site means by their nonmissing N.

## Fit a model only for people with follow-up

The model relates last to first haemoglobin only among the 25 people
with a later reading. Their first and last readings are about 6–18 years
apart. The summary below describes those actual intervals at each site.
Each site’s length minus its missing count gives the valid values, 11
and 14, which sum to the model’s 25 complete cases. Its quantiles
describe the spread, not the minimum and maximum.

``` r
dsBaseClient::ds.summary("D_pool$followup_days", datasources = two_conns)
```

    #> $north
    #> $north$class
    #> [1] "integer"
    #>
    #> $north$length
    #> [1] 14
    #>
    #> $north$`quantiles & mean`
    #>       5%      10%      25%      50%      75%      90%      95%     Mean
    #> 4056.500 4081.000 4340.000 5565.000 5853.500 5957.000 6100.500 5112.818
    #>
    #>
    #> $south
    #> $south$class
    #> [1] "integer"
    #>
    #> $south$length
    #> [1] 14
    #>
    #> $south$`quantiles & mean`
    #>      5%     10%     25%     50%     75%     90%     95%    Mean
    #> 2177.00 2636.90 4630.50 5565.00 5570.25 6108.90 6284.95 5012.00

``` r
dsBaseClient::ds.numNA("D_pool$followup_days", datasources = two_conns)
```

    #> $north
    #> [1] 3
    #>
    #> $south
    #> [1] 0

``` r
fit <- dsBaseClient::ds.glm("last ~ first", data = "D_pool", family = "gaussian", datasources = two_conns)
b <- fit$coefficients["first", "Estimate"]
se <- fit$coefficients["first", "Std. Error"]
data.frame(N = fit$Nvalid, df = fit$df, Estimate = b, Lower_95 = b - qt(.975, fit$df) * se,
  Upper_95 = b + qt(.975, fit$df) * se)
```

|  N | df | Estimate | Lower\_95 | Upper\_95 |
| -: | -: | -------: | --------: | --------: |
| 25 | 23 |  \-0.159 |   \-0.454 |     0.136 |

The slope is about −0.159: a 1 g/dL higher first reading is associated
with a 0.159 g/dL lower last reading in this small sample. Its 95% t
interval, −0.454 to 0.136, includes zero. Variable follow-up and
regression to the mean limit this comparison; it is not an effect of
treatment or a common-time trajectory.

## Exercises

**1. Transform a person mean.** Calculate log(mean haemoglobin). Is this
the mean of that person’s logged readings?

Solution 1

``` r
logged <- omop_recipe(variables = list(log_mean = omop_number(average,
  omop_number_spec(transform = omop_number_transform("log")))))
recipe_execute(logged, out = "D_log", conns = conns)
dsBaseClient::ds.summary("D_log$log_mean", datasources = conns)
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
    #> 2.557554 2.576166 2.623579 2.661968 2.730164 2.753634 2.767809 2.669345

The median is about 2.662, the log of the median person mean, 14.325.
Reduction precedes the transform: log of a mean generally differs from
mean of logs.

**2. Interpret observed attainment.** Summarise days until the first
observed haemoglobin ≥14. How do zero and missing differ?

Solution 2

``` r
dsBaseClient::ds.summary("D_change$target_days", datasources = conns)
```

    #> $site
    #> $site$class
    #> [1] "integer"
    #>
    #> $site$length
    #> [1] 28
    #>
    #> $site$`quantiles & mean`
    #>       5%      10%      25%      50%      75%      90%      95%     Mean
    #>    0.000    0.000    0.000  185.500 2192.750 3052.000 4166.750 1274.808

``` r
dsBaseClient::ds.numNA("D_change$target_days", datasources = conns)
```

    #> $site
    #> [1] 2

There are 26 observed attainments and two missing; median time among
observed attainments is 185.5 days. Zero means the first reading already
meets the rule. Missing means no qualifying reading was observed; use
follow-up and censoring for a time-to-event analysis rather than
replacing missing with zero or discarding those people.

**3. Keep a rounded copy beside the original.** Request the copy from
the output and summarise it. Must its overall mean have one decimal
place?

Solution 3

``` r
copy <- omop_recipe(variables = list(mean = average),
  outputs = omop_output("people", format = "wide", columns = c("mean", "rounded"),
    number_columns = list(rounded = omop_number_column("mean",
      number_spec = omop_number_spec(rounding = omop_number_round("decimals", 1))))))
recipe_execute(copy, out = "D_copy", conns = conns)
dsBaseClient::ds.summary("D_copy$rounded", datasources = conns)
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
    #> 12.90500 13.17000 13.77500 14.30000 15.32500 15.69000 15.96500 14.45714

The overall rounded mean is 14.457 g/dL. Each person’s copy has one
decimal, but averaging those values can produce more decimals. The
original mean remains available for analysis; rounding does not add
measurement precision.

## Recap

Define the lab, unit, time window and person-level reducer before
interpreting a number. Preserve missing follow-up, state threshold
boundaries, and distinguish site-quantile averages from pooled
person-level estimates.

**Next:** [15. Derived numeric
formulas](https://isglobal-brge.github.io/dsOMOPClient/articles/book-15-formulas.md).
