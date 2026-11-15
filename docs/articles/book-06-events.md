# 6\. Events, windows and reducers

**What you will learn**

Select records, set inclusive time windows, and reduce them to first,
last, count, ever, closest, any and all values.

**You need:** the client packages and the Opal login from [Getting
started](https://isglobal-brge.github.io/dsOMOPClient/articles/book-00-start.md).
This chapter uses `omop.synthea`, the public synthetic Synthea27Nj data.

## Start this chapter

Log in first:

``` r
library(dsOMOPClient)
conns <- ds.omop.login("https://localhost:8443", user = "administrator",
  password = "password", resource = "omop.synthea", server = "site")$conns
```

The connection is ready; `conns` identifies the server used by the
analysis calls.

## Define the records once

[Chapter 4](https://isglobal-brge.github.io/dsOMOPClient/articles/book-04-vocabulary.md)
identifies Hemoglobin \[Mass/volume\] in Blood as **3000963** and gram
per deciliter as **8713**. An *event* describes which records the server
should read. These simulated hemoglobin measurements have numeric values
recorded in grams per deciliter (g/dL). Concept IDs here are vocabulary
inputs; they are not patient identifiers.

``` r
measure <- omop_event("measurement", concepts = 3000963L,
  value = "value_as_number", unit = omop_unit(8713L))
```

The unit declaration accepts g/dL values; other units become missing
unless you declare a conversion. It also records the unit of the result.
Use an `omop_unit()` conversion when your study needs conversion.

## Set a public time window

A *window* is an inclusive range of days around an *anchor*, the
reference date. A fixed date works even when the population has no index
event.

``` r
history <- omop_window(NULL, NULL)
lookback <- omop_window(-365, 0, anchor = "2020-01-01")
```

`history` includes all recorded dates. `lookback` includes only the 365
days before 2020-01-01 and that date itself. We will compare the two
after extraction. An omitted window on an advanced reducer reads all
history; a simple-layer shortcut usually defaults to history through the
index. Choose explicitly.

## First and last values

A *reducer* turns many records into one value per person or episode.

``` r
first <- omop_variable(event = measure,
  reducer = omop_reduce("first_value", window = history))
last <- omop_variable(event = measure,
  reducer = omop_reduce("last_value", window = history))
```

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
```

The unit is still g/dL. Do not average numeric concept IDs or mixed
units.

## Count and ever

``` r
n <- omop_variable(event = measure,
  reducer = omop_reduce("count", window = history, distinct = "day"))
ever <- omop_variable(event = measure,
  reducer = omop_reduce("binary", window = history))
```

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
```

Ever tests whether a record exists. Any and all test a condition on its
values. Here any asks about readings at least 16 g/dL; all asks whether
every reading is at least 14 g/dL. Both are arbitrary teaching cutoffs,
not clinical reference limits. A missing value makes its comparison
unknown; a true value settles any, and a false value settles all.
Otherwise an unknown remains missing. Empty windows give any = 0 and all
= NA: without measurements there is no all-value result. This differs
from R’s `all(logical(0))`, which is TRUE.

## Assign the columns privately

``` r
reducers <- omop_recipe(variables = list(first = first, last = last,
  mean = mean_value, days = n, ever = ever, any_high = any_high, all_high = all_high),
  outputs = omop_output("people", format = "wide", grain = "person"))
recipe_execute(reducers, out = c(people = "D_reducers"), conns = conns)
```

The columns are now in `D_reducers` on the server. Request one approved
aggregate at a time:

``` r
dsBaseClient::ds.summary("D_reducers$first", datasources = conns)
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

``` r
dsBaseClient::ds.numNA("D_reducers$first", datasources = conns)
```

    #> $site
    #> [1] 0

All 28 people have a first nonmissing measurement. Their mean is 13.99
g/dL; this gives each person one observation regardless of how often
they were tested.

``` r
dsBaseClient::ds.summary("D_reducers$last", datasources = conns)
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

The last readings average 14.89 g/dL across the same 28 people. First
and last can coincide for someone with only one reading.

``` r
dsBaseClient::ds.summary("D_reducers$days", datasources = conns)
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

The mean is 3.86 measurement days per person, with a median of 4. A
frequency table of each exact count is withheld on this dataset because
some nonzero cells are below `nfilter.tab` (3); the entire table is
withheld, not just its small cells. The summary describes the
distribution of measurement frequency without listing all exact count
frequencies. See the table refusal once:

``` r
day_table <- dsBaseClient::ds.table("D_reducers$days", useNA = "no", datasources = conns)
day_table$validity.message
```

    #> [1] "All studies failed for reasons identified below"

The message withholds the table. It does not say that these people have
no measurements.

``` r
ever_result <- dsBaseClient::ds.table("D_reducers$ever", useNA = "no", datasources = conns)
ever_result$output.list$TABLES.COMBINED_all.sources_counts
```

    #> D_reducers$ever
    #>  1
    #> 28

Everyone in this public dataset has a hemoglobin measurement. A person
without one would have ever = 0 in this defined window.

``` r
any_result <- dsBaseClient::ds.table("D_reducers$any_high", useNA = "always", datasources = conns)
any_result$output.list$TABLES.COMBINED_all.sources_counts
```

    #> D_reducers$any_high
    #>  0  1 NA
    #>  9 19  0

The released counts show 19 people with at least one reading at or above
16 g/dL and 9 without one. A known qualifying measurement is enough for
any, even alongside an unknown value.

``` r
all_result <- dsBaseClient::ds.table("D_reducers$all_high", useNA = "always", datasources = conns)
all_result$output.list$TABLES.COMBINED_all.sources_counts
```

    #> D_reducers$all_high
    #>  0  1 NA
    #> 24  4  0

Four people have all readings at or above 14 g/dL; 24 do not. All
distinguishes unknown comparisons from an empty set of records.

## Read the mean summary

``` r
dsBaseClient::ds.summary("D_reducers$mean", datasources = conns)
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

The mean of the 28 person-level means is 14.47 g/dL. Averaging all
measurement rows instead would give more weight to people measured more
often.

## Restrict the last value to one year

``` r
recent <- omop_recipe(variables = list(last = omop_variable(event = measure,
  reducer = omop_reduce("last_value", window = lookback))))
recipe_execute(recent, out = "D_recent", conns = conns)
recent_mean <- dsBaseClient::ds.mean("D_recent$last", type = "combine", datasources = conns)
recent_missing <- dsBaseClient::ds.numNA("D_recent$last", datasources = conns)
data.frame(Mean_g_dL = recent_mean$Global.Mean[1, "EstimatedMean"],
  N_missing = recent_missing$site)
```

| Mean\_g\_dL | N\_missing |
| ----------: | ---------: |
|       14.85 |         24 |

The mean is 14.85 g/dL, but 24 of the 28 people have no reading in this
year. Only four contribute, compared with all 28 in the all-history
summary. We show a mean rather than quantiles for this small group.
People without a recent reading remain in the roster with missing
values.

## Value at the index

The `omop_study()` shortcut combines an index event, variables and
outputs in one recipe. Let the first hemoglobin measurement set the
anchor for each person. *Value at index* means closest within a chosen
tolerance; it need not occur on the index day.

``` r
at_index <- omop_study(measure, outputs = "baseline",
  baseline = list(hemoglobin = omop_value_at_index("measurement", 3000963L,
    tolerance = c(-30, 0), unit = 8713L)))
recipe_execute(at_index, out = c(baseline = "D_at_index"), conns = conns)
```

The tolerance excludes later measurements. The shortcut uses
`closest_value`; its default tolerance would include 90 days before and
30 days after.

``` r
dsBaseClient::ds.summary("D_at_index$hemoglobin", datasources = conns)
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

The mean is again 13.99 g/dL: because the index is the first hemoglobin
reading, the closest value in \[-30, 0\] is that same reading. This
equality follows from the chosen index; an index such as a diagnosis
need not coincide with a lab. Only people with an eligible hemoglobin
measurement enter this indexed study. If a required anchor is missing, a
window is undefined and every reducer gives `NA`, including count and
ever.

## Convert to a common unit before reducing

A unit declaration alone keeps values already in that unit and makes
other units’ values missing; an explicit conversion normalises them
before the reducer runs. This example converts g/dL to g/L, multiplying
by ten. These are the vocabulary names for the input and target units:

``` r
units <- ds.omop.concept.lookup(c(8713L, 8636L), conns = conns)
units$pooled[c("concept_id", "concept_name")]
```

| concept\_id | concept\_name      |
| ----------: | :----------------- |
|        8713 | gram per deciliter |
|        8636 | gram per liter     |

Concept 8636 is gram per liter; 8713 is gram per deciliter. The
conversion below changes the unit by multiplying by ten.

``` r
lab_gl <- omop_event("measurement", 3000963L,
  unit = omop_unit(8636L, convert = list("8713" = c(factor = 10, offset = 0))))
```

Rows already in g/L retain their value. Other undeclared units become
missing by default; they are not guessed. A declared conversion also
transforms reference ranges. Use the same target and conversion map
across sites before pooling.

``` r
converted <- omop_recipe(variables = list(mean_gl = omop_variable(event = lab_gl,
  reducer = omop_reduce("mean"))),
  outputs = omop_output("people", format = "wide", grain = "person"))
recipe_execute(converted, out = c(people = "D_converted"), conns = conns)
```

``` r
dsBaseClient::ds.summary("D_converted$mean_gl", datasources = conns)
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

The overall mean is 144.67 g/L, ten times 14.47 g/dL. This run uses the
dataset’s unchanged recorded units; it does not pretend that the source
already had mixed units.

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
recipe_execute(binned, out = c(output = "D_lab_bins"), conns = conns)
```

The band is an ordered factor directly on the server. Its order matters
for model contrasts.

``` r
bins_result <- dsBaseClient::ds.table("D_lab_bins$band", useNA = "always", datasources = conns)
bins_result$output.list$TABLES.COMBINED_all.sources_counts
```

    #> D_lab_bins$band
    #>       below 14 14 to below 15    15 or above             NA
    #>              9              9             10              0

There are 9 people below 14, 9 from 14 to below 15, and 10 at or above
15 g/dL. All nonzero cells meet the release threshold. You see the
checked counts, never the private rows. A different set of bins can
cause the entire table to be withheld; a refusal is not an empty or zero
table. The left boundary is included and the right boundary excluded in
these bins.

``` r
dsBaseClient::ds.table("D_lab_bins$high",
  useNA = "no", datasources = conns)$output.list$TABLES.COMBINED_all.sources_counts
```

    #> D_lab_bins$high
    #>  0  1
    #> 18 10

The high flag is 1 for the same 10 people in the top band and 0 for the
other 18. The ungrouped mean column remains available for numeric
analysis.

`abnormal_high` and `abnormal_low` reducers compare each measurement
with its recorded reference bounds. These hemoglobin data have no such
bounds, so their zero comparable-abnormal counts would not establish
normality. Study-defined flags are a different, explicitly chosen
definition.

## Try it

Add a count of measurement rows and compare its released mean and
quantiles with the measurement-day summary above.

Solution

``` r
row_count <- omop_recipe(variables = list(rows = omop_variable(event = measure,
  reducer = omop_reduce("count", window = history, distinct = "row"))),
  outputs = omop_output("people", format = "wide", grain = "person"))
recipe_execute(row_count, out = c(people = "D_row_count"), conns = conns)
dsBaseClient::ds.summary("D_row_count$rows", datasources = conns)
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

The row and day summaries are identical here because nobody has two
hemoglobin records on the same day. With duplicates, row counts would
count each record while day counts would count that date once.

## Another numeric exercise

Build a variable using `omop_number_change()` to calculate last minus
first for people with at least two readings. Is this the same quantity
as a slope?

Solution

``` r
change_recipe <- omop_recipe(variables = list(change = omop_number_change(measure)))
recipe_execute(change_recipe, out = c(output = "D_event_change"), conns = conns)
dsBaseClient::ds.summary("D_event_change$change", datasources = conns)
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
dsBaseClient::ds.numNA("D_event_change$change", datasources = conns)
```

    #> $site
    #> [1] 3

The mean change is 1.016 g/dL among 25 people; 3 are missing because
they have only one reading. It differs from 14.893 − 13.986 = 0.907
because those two means include all 28 people. Change is in g/dL,
without dividing by elapsed time. Native change needs two eligible
readings; a single reading stays missing rather than becoming zero. A
fitted slope uses all eligible dates and values and has units per day.
Continue with [Working with lab
values](https://isglobal-brge.github.io/dsOMOPClient/articles/book-14-labs.md)
for both methods and pooled analyses.

## Recap

An event chooses records, a window chooses time, and a reducer answers
one question. Empty records and an undefined anchor are different.

**Next:** [7. Populations and index
events](https://isglobal-brge.github.io/dsOMOPClient/articles/book-07-populations.md).
