# 6\. Events, windows and reducers

**What you will learn**

Select records, set inclusive time windows, and reduce them to first,
last, count, ever, closest, any and all values.

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

## Define the records once

An *event* describes which records the server should read. These
simulated hemoglobin measurements have numeric values recorded in grams
per deciliter (g/dL). Concept IDs here are vocabulary inputs; they are
not patient identifiers.

``` r
measure <- omop_event("measurement", concepts = 3000963L,
  value = "value_as_number", where = omop_where(unit_concept_id == 8713L))
class(measure)
```

    #> [1] "omop_event" "list"

The unit predicate keeps values recorded in that unit; it does not
convert units. Use an `omop_unit()` conversion when your study needs
conversion.

## Set a public time window

A *window* is an inclusive range of days around an *anchor*, the
reference date. A fixed date works even when the population has no index
event.

``` r
history <- omop_window(NULL, NULL, anchor = "2020-01-01")
class(history)
```

    #> [1] "omop_window" "list"

Both ends are unbounded here, so every recorded measurement is eligible.
For a one-year lookback use `omop_window(-365, 0, anchor =
"2020-01-01")`; that window includes both endpoint days. An omitted
window on an advanced reducer reads all history; a simple-layer shortcut
usually defaults to history through the index. Choose explicitly.

## First and last values

A *reducer* turns many records into one value per person or episode.

``` r
first <- omop_variable(event = measure,
  reducer = omop_reduce("first_value", window = history))
last <- omop_variable(event = measure,
  reducer = omop_reduce("last_value", window = history))
class(last)
```

    #> [1] "omop_variable" "list"

These select by date, source and record key, not by the size of the
value. By default they skip missing values. `missing = "keep"` chooses
the row first, so the selected row can give a missing value. With no
records, both give `NA`.

## Mean values

A mean uses all nonmissing numeric values in the window. It answers a
different question from choosing the first or last recorded value.

``` r
mean_value <- omop_variable(event = measure,
  reducer = omop_reduce("mean", window = history))
class(mean_value)
```

    #> [1] "omop_variable" "list"

The unit is still g/dL. Do not average numeric concept IDs or mixed
units.

## Count and ever

``` r
n <- omop_variable(event = measure,
  reducer = omop_reduce("count", window = history, distinct = "day"))
ever <- omop_variable(event = measure,
  reducer = omop_reduce("binary", window = history))
class(n)
```

    #> [1] "omop_variable" "list"

Count measures distinct measurement days here; `distinct = "row"` counts
records. Ever is 1 when any record exists and 0 otherwise. Neither
requires a nonmissing numeric value. In a defined empty window both
count and ever are zero.

## Any and all test the values

``` r
any_high <- omop_variable(event = measure,
  reducer = omop_reduce("any", window = history,
    predicate = omop_where(value >= 16)))
all_high <- omop_variable(event = measure,
  reducer = omop_reduce("all", window = history,
    predicate = omop_where(value >= 14)))
class(any_high)
```

    #> [1] "omop_variable" "list"

Ever tests whether a record exists. Any and all test a condition on its
values. Here any asks about readings at least 16 g/dL; all asks whether
every reading is at least 14 g/dL. Both are arbitrary teaching cutoffs,
not clinical reference limits. A missing value makes its comparison
unknown; a true value settles any, and a false value settles all.
Otherwise an unknown remains missing. Empty windows give any = 0 and all
= NA: without measurements there is no all-value result.

## Assign the columns privately

``` r
reducers <- omop_recipe(variables = list(first = first, last = last,
  mean = mean_value, days = n, ever = ever, any_high = any_high, all_high = all_high),
  outputs = omop_output("people", format = "wide", grain = "person"))
book_symbols(recipe_execute(reducers, out = c(people = "D_reducers"), conns = conns))
```

    #>       people 
    #> "D_reducers"

Only the symbol mapping returns. Request one approved aggregate at a
time:

``` r
first_result <- dsBaseClient::ds.summary("D_reducers$first", datasources = conns)
knitr::kable(book_summary(first_result))
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
first_result
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

This summarises the first nonmissing measurement per person.

``` r
last_result <- dsBaseClient::ds.summary("D_reducers$last", datasources = conns)
knitr::kable(book_summary(last_result))
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

Full aggregate response

``` r
last_result
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
    #> 12.34000 12.74000 13.60000 15.00000 16.25000 16.83000 17.03000 14.89286

Later readings can change the last-value summary. The source dataset
contains hemoglobin measurements for everyone in this practice dataset.

``` r
days_result <- dsBaseClient::ds.summary("D_reducers$days", datasources = conns)
knitr::kable(book_summary(days_result))
```

| Site | Statistic |    Value |
| :--- | :-------- | -------: |
| site | 5%        | 1.000000 |
| site | 10%       | 1.700000 |
| site | 25%       | 3.000000 |
| site | 50%       | 4.000000 |
| site | 75%       | 4.000000 |
| site | 90%       | 5.000000 |
| site | 95%       | 5.650000 |
| site | Mean      | 3.857143 |

Full aggregate response

``` r
days_result
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
    #> 1.000000 1.700000 3.000000 4.000000 4.000000 5.000000 5.650000 3.857143

This summarises one measurement-day count per person. A frequency table
of each exact count is withheld on this dataset because some nonzero
cells are below `nfilter.tab` (3); the entire table is withheld, not
just its small cells. An approved numeric summary answers the comparison
without requesting that table.

``` r
ever_result <- dsBaseClient::ds.table("D_reducers$ever", useNA = "no", datasources = conns)
knitr::kable(book_counts(ever_result))
```

| Category | Count |
| :------- | ----: |
| 1        |    28 |

Full aggregate response

``` r
ever_result
```

    #> $output.list
    #> $output.list$TABLE_rvar.by.study_row.props
    #>                study
    #> D_reducers$ever site
    #>               1    1
    #> 
    #> $output.list$TABLE_rvar.by.study_col.props
    #>                study
    #> D_reducers$ever site
    #>               1    1
    #> 
    #> $output.list$TABLE_rvar.by.study_counts
    #>                study
    #> D_reducers$ever site
    #>               1   28
    #> 
    #> $output.list$TABLES.COMBINED_all.sources_proportions
    #> D_reducers$ever
    #> 1 
    #> 1 
    #> 
    #> $output.list$TABLES.COMBINED_all.sources_counts
    #> D_reducers$ever
    #>  1 
    #> 28 
    #> 
    #> 
    #> $validity.message
    #> [1] "Data in all studies were valid"

Everyone in this public dataset has a hemoglobin measurement. A person
without one would have ever = 0 in this defined window.

``` r
any_result <- dsBaseClient::ds.table("D_reducers$any_high", useNA = "always", datasources = conns)
knitr::kable(book_counts(any_result))
```

| Category | Count |
| :------- | ----: |
| 0        |     9 |
| 1        |    19 |
| NA       |     0 |

Full aggregate response

``` r
any_result
```

    #> $output.list
    #> $output.list$TABLE_rvar.by.study_row.props
    #>                    study
    #> D_reducers$any_high site
    #>                  0     1
    #>                  1     1
    #>                  NA  NaN
    #> 
    #> $output.list$TABLE_rvar.by.study_col.props
    #>                    study
    #> D_reducers$any_high      site
    #>                  0  0.3214286
    #>                  1  0.6785714
    #>                  NA 0.0000000
    #> 
    #> $output.list$TABLE_rvar.by.study_counts
    #>                    study
    #> D_reducers$any_high site
    #>                  0     9
    #>                  1    19
    #>                  NA    0
    #> 
    #> $output.list$TABLES.COMBINED_all.sources_proportions
    #> D_reducers$any_high
    #>     0     1    NA 
    #> 0.321 0.679 0.000 
    #> 
    #> $output.list$TABLES.COMBINED_all.sources_counts
    #> D_reducers$any_high
    #>  0  1 NA 
    #>  9 19  0 
    #> 
    #> 
    #> $validity.message
    #> [1] "Data in all studies were valid"

The released counts show 19 people with at least one reading at or above
16 g/dL and 9 without one. A known qualifying measurement is enough for
any, even alongside an unknown value.

``` r
all_result <- dsBaseClient::ds.table("D_reducers$all_high", useNA = "always", datasources = conns)
knitr::kable(book_counts(all_result))
```

| Category | Count |
| :------- | ----: |
| 0        |    24 |
| 1        |     4 |
| NA       |     0 |

Full aggregate response

``` r
all_result
```

    #> $output.list
    #> $output.list$TABLE_rvar.by.study_row.props
    #>                    study
    #> D_reducers$all_high site
    #>                  0     1
    #>                  1     1
    #>                  NA  NaN
    #> 
    #> $output.list$TABLE_rvar.by.study_col.props
    #>                    study
    #> D_reducers$all_high      site
    #>                  0  0.8571429
    #>                  1  0.1428571
    #>                  NA 0.0000000
    #> 
    #> $output.list$TABLE_rvar.by.study_counts
    #>                    study
    #> D_reducers$all_high site
    #>                  0    24
    #>                  1     4
    #>                  NA    0
    #> 
    #> $output.list$TABLES.COMBINED_all.sources_proportions
    #> D_reducers$all_high
    #>     0     1    NA 
    #> 0.857 0.143 0.000 
    #> 
    #> $output.list$TABLES.COMBINED_all.sources_counts
    #> D_reducers$all_high
    #>  0  1 NA 
    #> 24  4  0 
    #> 
    #> 
    #> $validity.message
    #> [1] "Data in all studies were valid"

Four people have all readings at or above 14 g/dL; 24 do not. All
distinguishes unknown comparisons from an empty set of records.

## Read the mean summary

``` r
mean_result <- dsBaseClient::ds.summary("D_reducers$mean", datasources = conns)
knitr::kable(book_summary(mean_result))
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

Full aggregate response

``` r
mean_result
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
    #> 12.90500 13.14667 13.78500 14.32500 15.33542 15.70000 15.92375 14.46679

This summarises one mean per person. It is not the pooled mean of all
measurement rows, which would give more weight to people measured more
often.

## Value at the index

Now let the first hemoglobin measurement set a different anchor for each
person. *Value at index* means closest within a chosen tolerance; it
need not occur on the index day.

``` r
at_index <- omop_study(measure, outputs = "baseline",
  baseline = list(hemoglobin = omop_value_at_index("measurement", 3000963L,
    tolerance = c(-30, 0), unit = 8713L)))
book_symbols(recipe_execute(at_index, out = c(baseline = "D_at_index"), conns = conns))
```

    #>     baseline 
    #> "D_at_index"

The tolerance excludes later measurements. The shortcut uses
`closest_value`; its default tolerance would include 90 days before and
30 days after.

``` r
at_index_result <- dsBaseClient::ds.summary("D_at_index$hemoglobin", datasources = conns)
knitr::kable(book_summary(at_index_result))
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
at_index_result
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

Only people with an eligible hemoglobin measurement enter this indexed
study. If a required anchor is missing, a window is undefined and every
reducer gives `NA`, including count and ever.

## Convert to a common unit before reducing

A numeric unit filter only restricts records. An explicit conversion
normalises the values before the reducer runs. This example converts
g/dL to g/L, multiplying by ten. These are the vocabulary names for the
input and target units:

``` r
units <- ds.omop.concept.lookup(c(8713L, 8636L), conns = conns)
knitr::kable(units$pooled["concept_name"])
```

| concept\_name      |
| :----------------- |
| gram per deciliter |
| gram per liter     |

``` r
lab_gl <- omop_event("measurement", 3000963L,
  unit = omop_unit(8636L, convert = list("8713" = c(factor = 10, offset = 0))))
class(lab_gl)
```

    #> [1] "omop_event" "list"

Rows already in g/L retain their value. Other undeclared units become
missing by default; they are not guessed. A declared conversion also
transforms reference ranges. Use the same target and conversion map
across sites before pooling.

``` r
converted <- omop_recipe(variables = list(mean_gl = omop_variable(event = lab_gl,
  reducer = omop_reduce("mean"))),
  outputs = omop_output("people", format = "wide", grain = "person"))
book_symbols(recipe_execute(converted, out = c(people = "D_converted"), conns = conns))
```

    #>        people 
    #> "D_converted"

``` r
conversion_result <- dsBaseClient::ds.summary("D_converted$mean_gl", datasources = conns)
knitr::kable(book_summary(conversion_result))
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

Full aggregate response

``` r
conversion_result
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
    #> 129.0500 131.4667 137.8500 143.2500 153.3542 157.0000 159.2375 144.6679

The means are ten times the g/dL means. This run uses the dataset’s
unchanged recorded units; it does not pretend that the source already
had mixed units.

## Value bins and explicit high/low flags

A *bin* groups numeric values into a declared interval. These cutoffs
are arbitrary teaching choices, **not clinical reference ranges or
treatment thresholds**. We apply them to each person’s mean, privately.

``` r
mean_value <- omop_variable(event = measure, reducer = omop_reduce("mean"))
band_levels <- c("below 14", "14 to below 15", "15 or above")
binned <- omop_recipe(variables = list(mean = mean_value,
  band = omop_number_bins(mean_value, breaks = c(-Inf, 14, 15, Inf), labels = band_levels),
  high = omop_number_flag(mean_value, "ge", 15),
  low = omop_number_flag(mean_value, "lt", 14)))
book_symbols(recipe_execute(binned, out = c(output = "D_lab_bins"), conns = conns))
```

    #>       output 
    #> "D_lab_bins"

The band is an ordered factor directly on the server. No mutate-plus-cut
or subsequent factor conversion is needed. The type matters for model
contrasts.

``` r
dsBaseClient::ds.class("D_lab_bins$band", datasources = conns)
```

    #> $site
    #> [1] "dsomop_date_factor" "ordered"            "factor"

``` r
bins_result <- dsBaseClient::ds.table("D_lab_bins$band", useNA = "always", datasources = conns)
bin_display <- book_counts(bins_result)
bin_display$Category <- ordered(bin_display$Category, levels = band_levels)
knitr::kable(bin_display[order(bin_display$Category, na.last = TRUE), ])
```

| Category       | Count |
| :------------- | ----: |
| below 14       |     9 |
| 14 to below 15 |     9 |
| 15 or above    |    10 |
| NA             |     0 |

``` r
dsBaseClient::ds.mean("D_lab_bins$mean", type = "combine", datasources = conns)
```

    #> $Global.Mean
    #>                 EstimatedMean Nmissing Nvalid Ntotal
    #> studiesCombined      14.46679        0     28     28
    #> 
    #> $Nstudies
    #> [1] 1
    #> 
    #> $ValidityMessage
    #>      ValidityMessage 
    #> site "VALID ANALYSIS"

Full aggregate response

``` r
bins_result
```

    #> $output.list
    #> $output.list$TABLE_rvar.by.study_row.props
    #>                 study
    #> D_lab_bins$band  site
    #>   below 14          1
    #>   14 to below 15    1
    #>   15 or above       1
    #>   NA              NaN
    #> 
    #> $output.list$TABLE_rvar.by.study_col.props
    #>                 study
    #> D_lab_bins$band       site
    #>   below 14       0.3214286
    #>   14 to below 15 0.3214286
    #>   15 or above    0.3571429
    #>   NA             0.0000000
    #> 
    #> $output.list$TABLE_rvar.by.study_counts
    #>                 study
    #> D_lab_bins$band  site
    #>   below 14          9
    #>   14 to below 15    9
    #>   15 or above      10
    #>   NA                0
    #> 
    #> $output.list$TABLES.COMBINED_all.sources_proportions
    #> D_lab_bins$band
    #>       below 14 14 to below 15    15 or above             NA 
    #>          0.321          0.321          0.357          0.000 
    #> 
    #> $output.list$TABLES.COMBINED_all.sources_counts
    #> D_lab_bins$band
    #>       below 14 14 to below 15    15 or above             NA 
    #>              9              9             10              0 
    #> 
    #> 
    #> $validity.message
    #> [1] "Data in all studies were valid"

All nonzero cells in this worked example meet the release threshold. You
see the checked counts, never the private rows. A different set of bins
can cause the entire table to be withheld; a refusal is not an empty or
zero table. The left boundary is included and the right boundary
excluded in these bins.

## Recorded abnormal-high and abnormal-low counts

The dedicated reducers compare each measurement with its own
`range_high` or `range_low`. They count comparable records outside those
bounds.

``` r
ranges <- omop_recipe(variables = list(
  high_count = omop_variable(event = measure, reducer = omop_reduce("abnormal_high")),
  low_count = omop_variable(event = measure, reducer = omop_reduce("abnormal_low"))),
  outputs = omop_output("people", format = "wide", grain = "person"))
book_symbols(recipe_execute(ranges, out = c(people = "D_lab_ranges"), conns = conns))
```

    #>         people 
    #> "D_lab_ranges"

``` r
ranges_result <- dsBaseClient::ds.table("D_lab_ranges$high_count", useNA = "no", datasources = conns)
knitr::kable(book_counts(ranges_result))
```

| Category | Count |
| :------- | ----: |
| 0        |    28 |

Full aggregate response

``` r
ranges_result
```

    #> $output.list
    #> $output.list$TABLE_rvar.by.study_row.props
    #>                        study
    #> D_lab_ranges$high_count site
    #>                       0    1
    #> 
    #> $output.list$TABLE_rvar.by.study_col.props
    #>                        study
    #> D_lab_ranges$high_count site
    #>                       0    1
    #> 
    #> $output.list$TABLE_rvar.by.study_counts
    #>                        study
    #> D_lab_ranges$high_count site
    #>                       0   28
    #> 
    #> $output.list$TABLES.COMBINED_all.sources_proportions
    #> D_lab_ranges$high_count
    #> 0 
    #> 1 
    #> 
    #> $output.list$TABLES.COMBINED_all.sources_counts
    #> D_lab_ranges$high_count
    #>  0 
    #> 28 
    #> 
    #> 
    #> $validity.message
    #> [1] "Data in all studies were valid"

Synthea27Nj has no recorded hemoglobin reference bounds. Zero here means
no recorded comparable abnormal-high result; it is **not evidence of
normality**. The study-defined flags above remain a separate, explicit
definition. Never fill missing clinical reference ranges with an
invented normal interval.

## Try it

Add a count of measurement rows and compare its released mean and
quantiles with the measurement-day summary above.

Solution

``` r
row_count <- omop_recipe(variables = list(rows = omop_variable(event = measure,
  reducer = omop_reduce("count", window = history, distinct = "row"))),
  outputs = omop_output("people", format = "wide", grain = "person"))
book_symbols(recipe_execute(row_count, out = c(people = "D_row_count"), conns = conns))
```

    #>        people 
    #> "D_row_count"

``` r
solution_result <- dsBaseClient::ds.summary("D_row_count$rows", datasources = conns)
knitr::kable(book_summary(solution_result))
```

| Site | Statistic |    Value |
| :--- | :-------- | -------: |
| site | 5%        | 1.000000 |
| site | 10%       | 1.700000 |
| site | 25%       | 3.000000 |
| site | 50%       | 4.000000 |
| site | 75%       | 4.000000 |
| site | 90%       | 5.000000 |
| site | 95%       | 5.650000 |
| site | Mean      | 3.857143 |

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
    #>       5%      10%      25%      50%      75%      90%      95%     Mean 
    #> 1.000000 1.700000 3.000000 4.000000 4.000000 5.000000 5.650000 3.857143

Compare the released summaries: records on the same day would count
separately as rows, but once as a day. Their equality or difference is a
data property, not a guarantee of the reducers.

## Another numeric exercise

Using the Synthea columns already assigned, calculate last minus first
and summarise the differences. Is this the same quantity as a slope?

Solution

``` r
change_recipe <- omop_recipe(variables = list(change = omop_number_change(measure)))
book_symbols(recipe_execute(change_recipe, out = c(output = "D_event_change"), conns = conns))
```

    #>           output 
    #> "D_event_change"

``` r
event_change <- dsBaseClient::ds.summary("D_event_change$change", datasources = conns)
knitr::kable(book_summary(event_change))
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

This is change in g/dL, without dividing by elapsed time. Native change
needs two eligible readings; a single reading stays missing rather than
becoming zero. A fitted slope uses all eligible dates and values and has
units per day. Continue with [Working with lab
values](https://isglobal-brge.github.io/dsOMOPClient/articles/book-14-labs.md)
for both methods and pooled analyses.

## Exercise 2: a common variant

Inspect the unit declaration before executing a conversion. What
changes: the patients or the measurement coordinate?

Solution 2

``` r
class(lab_gl)
```

    #> [1] "omop_event" "list"

``` r
lab_gl$unit
```

    #> $target
    #> [1] 8636
    #> 
    #> $convert
    #> $convert$`8713`
    #> factor offset 
    #>     10      0 
    #> 
    #> 
    #> $unknown
    #> [1] "missing"
    #> 
    #> $censored
    #> [1] "keep"
    #> 
    #> attr(,"class")
    #> [1] "omop_unit" "list"

It changes the numeric coordinate under the declared conversion map, not
who the source records describe.

## Recap

An event chooses records, a window chooses time, and a reducer answers
one question. Empty records and an undefined anchor are different.

**Next:** [7. Populations and index
events](https://isglobal-brge.github.io/dsOMOPClient/articles/book-07-populations.md).
